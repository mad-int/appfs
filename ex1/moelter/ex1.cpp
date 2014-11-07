// Katharina Moelter

#include <iostream>
#include <fstream>

#include <string>
using std::string;
using std::ios;

#include <algorithm>
#include <vector>
using std::vector;

#include <cstdint>
#include <cstdlib>
#include <cstring>

// get filename to read
static string parseArguments(int argc, const char** argv)
{
    string filename("");
    for (int i = 1; i < argc; i++)
    {
        filename = string(argv[i]);
    }
    return filename;
}

// read little endian 32 bit integers
int32_t read_file32(std::istream& stream)
{
    uint8_t byte[4];
    stream.read((char *) byte, 4);
    return static_cast<int32_t>((byte[0]) | (byte[1] << 8) | (byte[2] << 16) | (byte[3] << 24));
}

// 
int main(int argc, const char** argv)
{
    string filename = parseArguments(argc, argv);
    /* Vectors are sequence containers representing arrays that can change in size, dynamic array. */
    std::vector<int32_t>elements;

    std::fstream f;
    f.open(filename, std::ios::in | std::ios::binary);

    if (f.is_open())
    {
        // count number of elements(numbers) to store in vector
        f.seekg(0, ios::end);
        int length = f.tellg();
        f.seekg(0, ios::beg);
        // 4 bytes per number
        int elemcount = length / 4;
        elements.reserve(elemcount);

        // read the data 4-byte-wise
        for (size_t i = 0; i < length; i += 4)
        {
            int32_t number = read_file32(f);
            // we are only interested non-negative numbers
            if (number >= 0)
            {
                elements.push_back(number);
            }
        }
        f.close();
    }

    // sort vector of numbers 
    sort(elements.begin(), elements.end());

    // print sorted vector of numbers, 1 number per line 
    int32_t prev = -1;
    for (vector<int32_t>::iterator it = elements.begin(); it != elements.end(); it++)
    {
        if (*it > prev)
        {
            std::cout << (*it) << std::endl;
            prev = *it;
        }
    }

    return 0;
}