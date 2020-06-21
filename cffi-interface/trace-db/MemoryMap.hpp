//
// MemoryMap.hpp -- Immutable object representing all memory
// allocations recorded by the program.  The memory map
// is represented internally as a skip-list, a legacy of the
// initial C implementation.
//

#ifndef __MEMORY_MAP_HPP
#define __MEMORY_MAP_HPP

#include <cstdlib>
#include <cstdint>

#include "TraceBufferSize.hpp"

#define SKIP_LIST_MAX_HEIGHT 32

typedef struct snode
{
    uint64_t key;
    uint64_t value;
    struct snode **next;
} snode;

class MemoryMap
{
private:
    unsigned int m_height;
    struct snode m_head;

    static unsigned int random_level() {
        unsigned int level = 1;
        while (rand() < RAND_MAX/2 && level < SKIP_LIST_MAX_HEIGHT)
            level++;
        return level;
    }

    void skipListRemove(uint64_t key) {
        snode *update[SKIP_LIST_MAX_HEIGHT + 1];
        snode *current = &m_head;

        for (int i = m_height; i >= 1; i--) {
            while (current->next[i]->key < key)
                current = current->next[i];
            update[i] = current;
        }

        current = current->next[1];
        if (current->key == key) {
            for (unsigned int i = 1; i <= m_height; i++) {
                if (update[i]->next[i] != current)
                    break;
                update[i]->next[i] = current->next[i];
            }
            delete[] current->next;
            delete current;

            while (m_height > 1
                   && m_head.next[m_height] == &m_head)
                m_height--;
        }
    }

    void skipListUpdate(uint64_t key, uint64_t value) {
        snode *existing = skipListFind(key);
        if (existing && existing->key == key) {
            existing->value = value;
        }
        else {
            unsigned int level = random_level();
            if (level > m_height) {
                m_height++;
                level = m_height;
            }

            snode *new_node = new snode;
            new_node->key = key;
            new_node->value = value;
            new_node->next = new snode*[level + 1];

            snode *current = &m_head;
            for (unsigned int i = m_height; i >= 1; i--) {
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

    struct snode* skipListFind(uint64_t key) const {
        // This value is used as a sentinel and can't be used as a key
        if (key == UINT64_MAX)
            return NULL;

        const snode *current = &m_head;
        for (int i = m_height; i >= 1; i--) {
            while (current->next[i]->key <= key) {
                current = current->next[i];
            }
        }
        if (current == &m_head)
            return NULL;
        else
            return (snode *)current;
    }

    void construct() {
        m_height = 1;
        m_head.key = UINT64_MAX;
        m_head.next = new snode*[SKIP_LIST_MAX_HEIGHT + 1];
        for (int i = 0; i <= SKIP_LIST_MAX_HEIGHT; i++) {
            m_head.next[i] = &m_head;
        }
    }

    void destruct() {
        snode *current = m_head.next[1];
        while(current != &m_head) {
            snode *next = current->next[1];
            delete[] current->next;
            delete current;
            current = next;
        }
        delete[] m_head.next;
    }
public:
    MemoryMap() {
        construct();
    }

    virtual ~MemoryMap() {
        destruct();
    }

    MemoryMap& operator=(const MemoryMap &other) = delete;
    MemoryMap(const MemoryMap &other) = delete;

    // Clear the memory map
    void clear() {
        destruct();
        construct();
    }

    // Update the memory map with new allocation information
    void updateMemoryMap(const TraceBufferSize &traceBufferSize) {
        if (traceBufferSize.getSize() == 0)
            skipListRemove(traceBufferSize.getAddress());
        else
            skipListUpdate(traceBufferSize.getAddress(),
                           traceBufferSize.getSize());
    }

    // Return the buffer size dynamically allocated from ADDRESS.
    // If no buffer is allocated at ADDRESS, return UINT64_MAX.
    uint64_t computeBufferSize(uint64_t address) const {
        uint64_t bufferSize = UINT64_MAX;

        snode *result;
        result = skipListFind(address);
        if (result) {
            uint64_t region_end = result->key + result->value;
            if (address <= region_end) {
                bufferSize = region_end - address;
            }
        }

        return bufferSize;
    }
};

#endif // __MEMORY_MAP_HPP
