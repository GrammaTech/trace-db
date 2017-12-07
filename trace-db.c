#include <stdlib.h>

#include "read-trace.h"
#include "utils.h"

#include "trace-db.h"

trace_db *create_db()
{
    return calloc(1, sizeof(trace_db));
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
        for (int i = 1; i <= list->height; i++) {
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
    for (int i = 0; i < point->n_sizes; i++) {
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

void add_trace(trace_db *db, trace_read_state *state)
{
    trace trace = { malloc(INITIAL_TRACE_SIZE * sizeof(trace_point)),
                    0, INITIAL_TRACE_SIZE };
    skip_list *memory_map = create_memory_map();

    /* Read trace points */
    trace_point point;
    while (read_trace_point(state, &point) == 0) {
        update_memory_map(memory_map, &point);
        for (int i = 0; i < point.n_vars; i++) {
            compute_buffer_size(memory_map, state, &point.vars[i]);
        }

        /* Copy contents out of shared state buffers */
        point.sizes = malloc_copy(point.sizes,
                                  point.n_sizes * sizeof(trace_buffer_size));
        point.vars = malloc_copy(point.vars,
                                 point.n_vars * sizeof(trace_var_info));
        point.aux = malloc_copy(point.aux, point.n_aux * sizeof(*point.aux));

        ensure_buffer_size((void **)&trace.points, sizeof(trace_point),
                           &trace.n_points_allocated, trace.n_points + 1);
        trace.points[trace.n_points++] = point;
    }

    free_memory_map(memory_map);

    /* Move names and types from state into trace */
    trace.names = state->names;
    trace.n_names = state->n_names;
    state->names = NULL;

    trace.types = state->types;
    trace.n_types = state->n_types;
    state->types = NULL;

    end_reading(state);

    /* Store trace */
    ensure_buffer_size((void **)&db->traces, sizeof(trace),
                       &db->n_traces_allocated, db->n_traces + 1);
    db->traces[db->n_traces++] = trace;
}

void free_db(trace_db *db)
{
    for (int i = 0; i < db->n_traces; i++) {
        if (db->traces[i].n_points > 0)
            free(db->traces[i].points);
        free(db->traces);
    }
    free(db);
}
