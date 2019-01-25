#ifndef ptscan_h
#define ptscan_h

#include <cstdarg>
#include <cstdint>
#include <cstdlib>

namespace PtScan {

struct ProcessHandle;

struct ScanResult;

struct Scanner;

struct ScannerResults;

struct ThreadPool;

extern "C" {

/// Free the ProcessHandle.
void ptscan_process_handle_free(ProcessHandle *process_handle);

/// Find a process by name.
/// If a process cannot be found, *out is left as NULL.
bool ptscan_process_handle_open_by_name(const char *name, uintptr_t name_len, ProcessHandle **out);

void ptscan_scanner_free(Scanner *scanner);

bool ptscan_scanner_new(const ThreadPool *thread_pool, Scanner **out);

void ptscan_scanner_results_free(ScannerResults *scanner_results);

void ptscan_scanner_results_iter(Scanner *scanner, ScannerResults **out);

void ptscan_scanner_results_next(ScannerResults *scanner_results, ScanResult **out);

void ptscan_thread_pool_free(ThreadPool *thread_pool);

bool ptscan_thread_pool_new(ThreadPool **out);

} // extern "C"

} // namespace PtScan

#endif // ptscan_h
