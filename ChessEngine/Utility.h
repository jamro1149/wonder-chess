// noexcept macro for compilers without noexcept *cough cough VC12 cough*
#if NOEXCEPT_SUPPORTED
#define NOEXCEPT noexcept
#else
#define NOEXCEPT throw()
#endif

#include <algorithm>
#include <vector>

namespace Chess
{
namespace Utility
{
template <typename T>
void InplaceUnion(std::vector<T>& a, const std::vector<T>& b)
{
    int mid = a.size();

    a.insert(a.end(), b.begin(), b.end());

    std::inplace_merge(a.begin(), a.begin() + mid, a.end());

    a.erase(std::unique(a.begin(), a.end()), a.end());
}

template <typename T> bool BinarySearch(const std::vector<T>& v, const T& value)
{
    return std::binary_search(v.begin(), v.end(), value);
}

template <typename T> bool LinearSearch(const std::vector<T>& v, const T& value)
{
    return std::find(v.begin(), v.end(), value) != v.end();
}
} // namespace Utility
} // namespace Chess
