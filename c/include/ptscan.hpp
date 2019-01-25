#ifndef ptscan_h
#define ptscan_h

#include <cstdarg>
#include <cstdint>
#include <cstdlib>

namespace PtScan {

struct ProcessHandle;

/// A single scan result.
struct ScanResult;

/// A scanner keeping track of results scanned from memory.
struct Scanner;

/// An iterator over scan results.
struct ScannerResultsIter;

struct ThreadPool;

extern "C" {

/// Close and free the ProcessHandle.
void ptscan_process_handle_free(ProcessHandle *process_handle);

/// Find a process by name.
/// If a process cannot be found, *out is set to NULL.
bool ptscan_process_handle_open_by_name(const char *name, uintptr_t name_len, ProcessHandle **out);

/// Close and free the scanner.
void ptscan_scanner_free(Scanner *scanner);

bool ptscan_scanner_new(const ThreadPool *thread_pool, Scanner **out);

/// Free the scanner results iterator.
void ptscan_scanner_results_free(ScannerResultsIter *scanner_results);

/// Create an iterator over the results of a scan.
/// # Safety
/// Modifying a collection while an iterate is open results in undefined behavior.
void ptscan_scanner_results_iter(Scanner *scanner, ScannerResultsIter **out);

/// Walk the iterator one step.
/// If no more elements are available *out is set to NULL, otherwise it is set to point to the next element.
void ptscan_scanner_results_next(ScannerResultsIter *scanner_results,
                                 ScanResult **out);

/// Close and free the thread pool.
void ptscan_thread_pool_free(ThreadPool *thread_pool);

/// Create a new thread pool.
bool ptscan_thread_pool_new(ThreadPool **out);

} // extern "C"

} // namespace PtScan

#endif // ptscan_h
