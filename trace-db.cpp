#include <assert.h>
#include <cstdlib>
#include <cstring>
#include <vector>

extern "C" {
#include "read-trace.h"
#include "utils.h"
}

#include "trace-db.h"

trace_db *create_db()
{
    return (trace_db*)calloc(1, sizeof(trace_db));
}

#define SKIP_LIST_MAX_HEIGHT 32
typedef struct snode
{
    uint64_t key;
    uint64_t value;
    struct snode **next;
} snode;

typedef struct skip_list
{
    unsigned int height;
    struct snode head;
} skip_list;

static int random_level()
{
    int level = 1;
    while (rand() < RAND_MAX/2 && level < SKIP_LIST_MAX_HEIGHT)
        level++;
    return level;
}

/* Remove entry from skip list */
static void skip_list_remove(skip_list *list, uint64_t key)
{
    snode *update[SKIP_LIST_MAX_HEIGHT + 1];
    snode *current = &list->head;

    for (int i = list->height; i >= 1; i--) {
        while (current->next[i]->key < key)
            current = current->next[i];
        update[i] = current;
    }

    current = current->next[1];
    if (current->key == key) {
        for (unsigned int i = 1; i <= list->height; i++) {
            if (update[i]->next[i] != current)
                break;
            update[i]->next[1] = current->next[i];
        }
        free(current->next);
        free(current);

        while (list->height > 1
               && list->head.next[list->height] == &list->head)
            list->height--;
    }
}

/* Return node with largest key less than or equal to desired key. */
static snode *skip_list_find(const skip_list *list, uint64_t key)
{
    const snode *current = &list->head;
    for (int i = list->height; i >= 1; i--) {
        while (current->next[i]->key <= key) {
            current = current->next[i];
        }
    }
    if (current == &list->head)
        return NULL;
    else
        return (snode *)current;
}

/* Update existing entry in skip list, or insert a new entry if none
   exists. */
static void skip_list_update(skip_list *list, uint64_t key, uint64_t value)
{
    snode *existing = skip_list_find(list, key);
    if (existing && existing->key == key) {
        existing->value = value;
    }
    else {
        int level = random_level();
        snode *new_node = (snode *)malloc(sizeof(snode));
        new_node->key = key;
        new_node->value = value;
        new_node->next = (snode **)malloc(sizeof(snode*) * (level + 1));

        snode *current = &list->head;
        for (int i = list->height; i >= 1; i--) {
            while (current->next[i]->key < key) {
                current = current->next[i];
            }

            if (i <= level) {
                new_node->next[i] = current->next[i];
                current->next[i] = new_node;
            }
        }
    }
}

skip_list *create_memory_map()
{
    skip_list *memory_map = (skip_list *)malloc(sizeof(skip_list));
    memory_map->height = 1;
    memory_map->head.key = UINT64_MAX;
    memory_map->head.next = (snode **)malloc(sizeof(snode*) * (SKIP_LIST_MAX_HEIGHT + 1));
    for (int i = 0; i <= SKIP_LIST_MAX_HEIGHT; i++) {
        memory_map->head.next[i] = &memory_map->head;
    }

    return memory_map;
}

void free_memory_map(skip_list *list)
{
    snode *current = list->head.next[1];
    while(current != &list->head) {
        snode *next = current->next[1];
        free(current->next);
        free(current);
        current = next;
    }
    free(list);
}

void update_memory_map(skip_list *memory_map, const trace_point *point)
{
    for (uint32_t i = 0; i < point->n_sizes; i++) {
        trace_buffer_size bsize = point->sizes[i];
        if (bsize.size == 0)
            skip_list_remove(memory_map, bsize.address);
        else
            skip_list_update(memory_map, bsize.address, bsize.size);
    }
}

void compute_buffer_size(const skip_list *memory_map,
                         const trace_read_state *state,
                         trace_var_info *var)
{
    var->has_buffer_size = 0;

    snode *result;
    uint64_t address = (uint64_t)var->value.ptr;
    if (state->types[var->type_index].format == POINTER
        && (result = skip_list_find(memory_map, address))) {
        uint64_t region_end = result->key + result->value;
        if (address < region_end) {
            var->buffer_size = region_end - address;
            var->has_buffer_size = 1;
        }
    }
}

#define INITIAL_TRACE_SIZE (1 << 16)

void add_trace(trace_db *db, trace_read_state *state, uint64_t max)
{
    if (max == 0)
        max = UINT64_MAX;

    trace_point *points =
        (trace_point*)malloc(INITIAL_TRACE_SIZE * sizeof(trace_point));
    trace new_trace = { points, 0, INITIAL_TRACE_SIZE };
    skip_list *memory_map = create_memory_map();

    /* Read trace points */
    trace_point point;
    for (uint64_t i = 0; i < max && (read_trace_point(state, &point) == 0); i++) {
        update_memory_map(memory_map, &point);
        for (uint32_t i = 0; i < point.n_vars; i++) {
            compute_buffer_size(memory_map, state, &point.vars[i]);
        }

        /* Copy contents out of shared state buffers */
        point.sizes =
            (trace_buffer_size*)malloc_copy(point.sizes,
                                             point.n_sizes * sizeof(trace_buffer_size));
        point.vars = (trace_var_info*)malloc_copy(point.vars,
                                                   point.n_vars * sizeof(trace_var_info));
        point.aux = (uint64_t*)malloc_copy(point.aux, point.n_aux * sizeof(*point.aux));

        void *tmp = new_trace.points;
        ENSURE_BUFFER_SIZE(tmp, sizeof(trace_point),
                           new_trace.n_points_allocated, new_trace.n_points + 1);
        new_trace.points = (trace_point*)tmp;
        new_trace.points[new_trace.n_points++] = point;
    }

    free_memory_map(memory_map);

    /* Move names and types from state into trace */
    new_trace.names = state->names;
    new_trace.n_names = state->n_names;
    state->names = NULL;

    new_trace.types = state->types;
    new_trace.n_types = state->n_types;
    state->types = NULL;

    end_reading(state);

    /* Store trace */
    void *tmp = db->traces;
    ENSURE_BUFFER_SIZE(tmp, sizeof(trace),
                       db->n_traces_allocated, db->n_traces + 1);
    db->traces = (trace*) tmp;
    db->traces[db->n_traces++] = new_trace;
}

void free_db(trace_db *db)
{
    for (uint64_t i = 0; i < db->n_traces; i++) {
        if (db->traces[i].n_points > 0)
            free(db->traces[i].points);
        free(db->traces);
    }
    free(db);
}

static bool type_compatible(const free_variable &free_var,
                            const trace_var_info &real_var)
{
    for (uint32_t i = 0; i < free_var.n_allowed_types; i++) {
        if (free_var.allowed_types[i] == real_var.type_index)
            return true;
    }
    return false;
}

// Cartesian product of vectors
static std::vector<std::vector<uint32_t> >
cartesian(const std::vector<std::vector<uint32_t> > &vectors)
{
    std::vector<std::vector<uint32_t> > results;
    std::vector<uint32_t> current(vectors.size());

    size_t n_results = 1;
    for (auto v : vectors)
        n_results *= v.size();
    results.reserve(n_results);

    // Enumerate all results, using modular arithmetic to index into
    // individual vectors.
    for (size_t result_i = 0; result_i < n_results; result_i++) {
        size_t quotient = result_i;
        for (size_t v_i = 0; v_i < vectors.size(); v_i++) {
            auto v = vectors[v_i];
            size_t n = quotient % v.size();
            current[v_i] = v[n];
            quotient /= v.size();
        }
        results.push_back(current);
    }

    return results;
}

struct int_value
{
    bool is_signed;
    bool is_valid;
    union {
        uint64_t u;
        int64_t s;
    } value;

    int_value(uint64_t val)
        : is_signed(false), is_valid(true)
    {
        value.u = val;
    }

    int_value(int64_t val)
        : is_signed(true), is_valid(true)
    {
        value.s = val;
    }

    int_value() : is_valid(false)
    {}

    bool greater_than(const int_value &other)
    {
        if (!(is_valid && other.is_valid))
            return false;

        if (is_signed) {
            if (other.is_signed)
                return value.s > other.value.s;
            else
                return value.s >= 0 && (uint64_t)value.s > other.value.u;
        }
        else {
            if (other.is_signed)
                return other.value.s < 0 || value.u > (uint64_t)other.value.s;
            else
                return value.u > other.value.u;
        }
    }

    bool less_than(const int_value &other)
    {
        if (!(is_valid && other.is_valid))
            return false;

        if (is_signed) {
            if (other.is_signed)
                return value.s < other.value.s;
            else
                return value.s < 0 || (uint64_t)value.s < other.value.u;
        }
        else {
            if (other.is_signed)
                return other.value.s >= 0 && value.u < (uint64_t)other.value.s;
            else
                return value.u < other.value.u;
        }
    }
};

static const trace_var_info &var_lookup(const trace &trace,
                                        const trace_point &point,
                                        const std::vector<uint32_t> &bindings,
                                        int var_index)
{
    return point.vars[bindings[var_index]];
}

static int_value evaluate(const trace &trace,
                          const trace_point &point,
                          const std::vector<uint32_t> &bindings,
                          const predicate *predicate)
{
    switch (predicate->kind) {
    case VAR_SIZE:
        {
            assert(predicate->data.n_children == 1);
            const struct predicate &c = predicate->children[0];
            assert(c.kind == VAR_REFERENCE);
            const trace_var_info &var = var_lookup(trace, point, bindings,
                                                   c.data.var_index);
            return var.has_buffer_size ? int_value(var.buffer_size)
                : int_value();
        }

    case VAR_VALUE:
        {
            assert(predicate->data.n_children == 1);
            const struct predicate &c = predicate->children[0];
            assert(c.kind == VAR_REFERENCE);
            const trace_var_info &var = var_lookup(trace, point, bindings,
                                                   c.data.var_index);
            enum type_format f = trace.types[var.type_index].format;
            if (f == UNSIGNED)
                return int_value(var.value.u);
            else if (f == SIGNED)
                return int_value(var.value.s);
            else
                return int_value();
        }
    default:
      // Should never reach var reference or logical operators here
      assert(false);
    }

}

static bool satisfies_predicate(const trace &trace,
                                const trace_point &point,
                                const std::vector<uint32_t> &bindings,
                                const predicate *predicate)
{
    // Trivial predicate
    if (predicate == NULL)
        return true;

    switch (predicate->kind) {
    case AND:
      for (uint32_t i = 0; i < predicate->data.n_children; i++) {
          if (!satisfies_predicate(trace, point, bindings, &predicate->children[i]))
              return false;
      }
      return true;
    case OR:
      for (uint32_t i = 0; i < predicate->data.n_children; i++) {
          if (satisfies_predicate(trace, point, bindings, &predicate->children[i]))
              return true;
      }
      return false;
    case DISTINCT_VARS:
        {
            assert(predicate->data.n_children == 2);
            const struct predicate &c0 = predicate->children[0];
            const struct predicate &c1 = predicate->children[1];

            assert(c0.kind == VAR_REFERENCE);
            assert(c1.kind == VAR_REFERENCE);
            return (bindings[c0.data.var_index] != bindings[c1.data.var_index]);
        }
    case GREATER_THAN:
        {
            assert(predicate->data.n_children == 2);
            int_value v0 = evaluate(trace, point, bindings, &predicate->children[0]);
            int_value v1 = evaluate(trace, point, bindings, &predicate->children[1]);

            return v0.greater_than(v1);
        }
    case LESS_THAN:
        {
            assert(predicate->data.n_children == 2);
            int_value v0 = evaluate(trace, point, bindings, &predicate->children[0]);
            int_value v1 = evaluate(trace, point, bindings, &predicate->children[1]);

            return v0.less_than(v1);
        }
    default:
      // should not reach var references or arithmetic expressions here
      assert(false);
    }

    // We should never reach this point. All cases must return a value.
    assert(false);
}

static void print_predicate(FILE *stream, const predicate *predicate,
                            int indent=0, bool do_indent=false)
{
    if (!predicate)
        return;

    if (do_indent)
        fprintf(stream, "%*c", indent - 1, ' ');

    if (predicate->kind == VAR_REFERENCE) {
        fprintf(stream, "<v%u>", predicate->data.var_index);
    }
    else {
        const char *name;
        switch (predicate->kind) {
        case VAR_SIZE:
          name = "v/size";
          break;
        case VAR_VALUE:
          name = "v/value";
          break;
        case AND:
          name = "and";
          break;
        case OR:
          name = "or";
          break;
        case DISTINCT_VARS:
          name = "distinct";
          break;
        case LESS_THAN:
          name = "<";
          break;
        case GREATER_THAN:
          name = ">";
          break;
        default:
          assert(false);
        }
        indent += fprintf(stream, "(%s ", name);

        for (uint32_t i = 0; i < predicate->data.n_children; i++)
        {
            print_predicate(stream, &predicate->children[i], indent,
                            i != 0);
            fprintf(stream, (i == predicate->data.n_children - 1) ? ")" : "\n");
        }
    }
    if (indent == 0)
        fprintf(stream, "\n");
}

// Find matching variable bindings at a single trace point, and push them
// onto vector results_out.
static void collect_results_at_point(const trace &trace,
                                     const trace_point &current,
                                     uint32_t n_variables,
                                     const free_variable *variables,
                                     const predicate *predicate,
                                     std::vector<trace_point> *results_out)
{
    // Find possible bindings for each free variable
    std::vector<std::vector<uint32_t> > matching_vars(n_variables);
    for (uint32_t free_var_i = 0; free_var_i < n_variables; free_var_i++) {
        for (uint32_t point_var_i = 0; point_var_i < current.n_vars; point_var_i++) {
            if (type_compatible(variables[free_var_i],
                                current.vars[point_var_i]))
                matching_vars[free_var_i].push_back(point_var_i);
        }
    }

    // Collect all combinations of bindings
    for (auto bindings : cartesian(matching_vars)) {
        if (satisfies_predicate(trace, current, bindings, predicate)) {
            trace_point point = { current.statement };
            point.n_vars = bindings.size();
            point.vars = (trace_var_info *)malloc(point.n_vars * sizeof(trace_var_info));
            for (uint32_t i = 0; i < point.n_vars; i++) {
                point.vars[i] = current.vars[bindings[i]];
            }
            results_out->push_back(point);
        }
    }
}

// Allocate an array and fill it with the contents of result vector.
static void results_vector_to_array(const std::vector<trace_point> results,
                                    trace_point **results_out,
                                    uint64_t *n_results_out)
{
    // Allocate buffer and return a copy of the results
    size_t results_size = results.size() * sizeof(trace_point);
    if (results_size > 0) {
        *results_out = (trace_point*)malloc(results_size);
        memcpy(*results_out, &results[0], results_size);
    }
    else {
        *results_out = NULL;
    }
    *n_results_out = results.size();
}

void query_trace(const trace_db *db, uint64_t index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *predicate,
                 trace_point **results_out, uint64_t *n_results_out)
{
    assert(index < db->n_traces);
    std::vector<trace_point> results;
    const trace &trace = db->traces[index];

    for (uint32_t point_i = 0; point_i < trace.n_points; point_i++) {
        collect_results_at_point(trace, trace.points[point_i], n_variables,
                                 variables, predicate, &results);
    }

    results_vector_to_array(results, results_out, n_results_out);
}

void query_point(const trace_db *db, uint64_t trace_index, uint64_t point_index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *predicate,
                 trace_point **results_out, uint64_t *n_results_out)
{
    assert(trace_index < db->n_traces);
    const trace &trace = db->traces[trace_index];
    assert(point_index < trace.n_points);

    std::vector<trace_point> results;
    collect_results_at_point(trace, trace.points[point_index], n_variables,
                             variables, predicate, &results);
    results_vector_to_array(results, results_out, n_results_out);
}


void free_query_result(trace_point *results, uint64_t n_results)
{
    for (uint64_t i = 0; i < n_results; i++)
        free(results[i].vars);
    if (n_results > 0)
        free(results);
}

static void free_predicate_helper(predicate *predicate)
{
    if (predicate->kind != VAR_REFERENCE) {
        for (uint32_t i = 0; i < predicate->data.n_children; i++)
            free_predicate_helper(&predicate->children[i]);
        free(predicate->children);
    }
}

void free_predicate(predicate *predicate)
{
    if (predicate != NULL) {
        free_predicate_helper(predicate);
        free(predicate);
    }
}
