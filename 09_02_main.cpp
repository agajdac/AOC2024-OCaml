#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <deque>

int main()
{
    std::vector<int> data;
    std::deque<int> nodes;

    std::ifstream file("data.txt");
    std::size_t sum = 0;

    char c;
    while (file.get(c))
    {
        data.push_back(static_cast<int>(c) - '0');
        sum += static_cast<int>(c) - '0';
    }

    int index = 0;
    int counter = 0;
    
    for(int i = 0; i < data.size(); ++i)
    {
        int x = data[i];
        while(x > 0)
        {
            --x;
            if(i % 2 == 0)
            {
                nodes.push_back(index);
            }
            else
            {
                nodes.push_back(-1);
            }
        }
        if(i % 2 == 0)
        {
            ++index;
        }
    }

    int r = nodes.size() - 1;
    while(r >= 0)
    {
        while(r >= 0 && nodes[r] < 0)
        {
            --r;
        }

        int first = nodes[r];
        int rC = 0;
        while(r >= 0 && nodes[r] == first)
        {
            --r;
            ++rC;
        }

        int l = 0;
        int lC = 0;
        while(l <= r)
        {
            lC = 0;
            while(l <= r && nodes[l] != -1)
            {
                ++l;
            }

            while(l <= r && nodes[l] == -1)
            {
                ++lC;
                ++l;
                if(lC == rC)
                {
                    for(int i = 0; i < lC; ++i)
                    {
                        std::swap(nodes[l - i - 1], nodes[r + i + 1]);
                    }
                    break;
                }
            }
        }
    }


    // checksum
    long long unsigned ans = 0;
    for(int i = 0; i < nodes.size(); ++i)
    {
        if(nodes[i] != -1)
        {
            ans += i * nodes[i];
        }
    }
    std::cout << ans << '\n';

    return 0;
}
