#include<stdlib.h>
#include<iostream>
#include<fstream>
#include<vector>
#include<string>

#define TILE_WIDTH 16  
#define MAXPOOL_INPUT_FILENAME "input.txt"
#define A_FILENAME "a.txt"
#define B_FILENAME "b.txt"
#define C_FILENAME "c.txt"

using namespace std;

__global__ void maxpool(float *input, float *output, const int input_size, const int filter_size) {
    // input : input_matrix address
    // output : output buffer address
    // input_size : width, height of input matrix
    // filter_size : filter_size of maxpolling
    // all input, output matrices are vectorized

    int col = blockDim.x * blockIdx.x + threadIdx.x;
    int row = blockDim.y * blockIdx.y + threadIdx.y;

    int output_size = input_size / filter_size;

    // out of bound
    if (col >= output_size || row >= output_size)
    {
        return;
    }
    
    // 2D to 1D : (row, col) -> (row * N) + col
    float max_val = input[((row * filter_size) * input_size) + (col * filter_size)];

    for (int i = row * filter_size; i < row * filter_size + filter_size; i++)
    {
        for (int j = col * filter_size; j < col * filter_size + filter_size; j++)
        {
            // update max_val
            max_val = fmaxf(max_val, input[(i * input_size) + j]);
        }
    }
    // assign max value
    output[(row * output_size) + col] = max_val;
}

__global__ void gemm(float *a, float *b, float *c, const float alpha, const float beta, float *output, const int input_size){
    // a, b, c : input matrix address
    // alpha, beta : input constant
    // output : output buffer address
    // input_size : width, height of input matrix
    // all input, output matrices are vectorized

    int tx = threadIdx.x, ty = threadIdx.y;
    int bx = blockIdx.x,  by = blockIdx.y;
    int i = blockIdx.x * TILE_WIDTH, j = blockIdx.y * TILE_WIDTH;//add

    int row = by*blockDim.y + ty;
    int col = bx*blockDim.x + tx;
    
    if(row>=input_size ||col>=input_size){
        return;
    }
    
    // allocate 2D tiles in __shared__ memory
    __shared__ float s_a[TILE_WIDTH][TILE_WIDTH];
    __shared__ float s_b[TILE_WIDTH][TILE_WIDTH];
    __shared__ float s_c[TILE_WIDTH][TILE_WIDTH];

    float resultValue = 0.0f;

    // make sure you handle the case when the matrix sizes are not
    // multiple of TILE_WIDTH!
    // loop over the tiles of the input in phases
    for(int p = 0; p < ceilf(input_size/TILE_WIDTH)+1; p++)
    {
        // CHANGE //////////////////////////////////////////////////
        s_a[ty][tx] = 0.0f; // to ignore uneffected values

        // boundary check
        if (row < input_size && (TILE_WIDTH * p + tx) < input_size)
        {
            s_a[ty][tx] = a[row * input_size + TILE_WIDTH * p + tx];
        }

        s_b[ty][tx] = 0.0f; // to ignore uneffected values

        // boundary check
        if (col < input_size && (p * TILE_WIDTH + ty) < input_size)
        {
            s_b[ty][tx] = b[(p * TILE_WIDTH + ty) * input_size + col];
        }
        __syncthreads(); // barrier

        for (int j = 0; j<TILE_WIDTH; j++)
        {
            resultValue += s_a[ty][j] * s_b[j][tx]; // get tile sum for block
        }
        __syncthreads(); // barrier
        // You need to use __syncthreads() a few times
        // to synchronize the threads in a thread block.
    }

    // write out the result to output[row*input_size + col] 
    // CHANGE //////////////////////////////////////////////////
    if (row < input_size && col < input_size)
    {
        int index = (i + tx) + (j + ty)*input_size;
        s_c[ty][tx] = c[index];
        output[index] = alpha * resultValue + beta * s_c[ty][tx];
    }
}


int main(int argc, char **argv)
{
    if(argc < 4) {//check
        cout << "usage : " << argv[0] << " input_size filter_size alpha beta\n" << "example : " << argv[0] << " 100 2 0.5 0.8\n";
        return 1;
    }
    const int input_size = stoi(argv[1]);
    const int filter_size = stoi(argv[2]); // used for maxpooling//check
    const float alpha = stof(argv[3]);
    const float beta = stof(argv[4]);
    const int maxpool_output_size = input_size/filter_size;//check

    // check input_size is power of 2 //16?
    if(input_size == 0 && (input_size & (input_size-1)))
    {
        cout << "input_size must be power of 2\n";//16??
        return 1;
    }

    if(filter_size == 0)
    {
        cout << "filter_size cannot be 0\n";
        return 1;
    }

    //initialize host arrays (array defined in C++)
    float h_maxpool_input[input_size*input_size]; // pointer?
    float h_a[input_size*input_size];
    float h_b[input_size*input_size];
    float h_c[input_size*input_size];
    
    // read input matrices 
    ifstream input_in(MAXPOOL_INPUT_FILENAME); //check ifstream
    ifstream a_in(A_FILENAME);
    ifstream b_in(B_FILENAME);
    ifstream c_in(C_FILENAME);

    //transfer the stream input to host arrays
    for (int i = 0; i < input_size*input_size; ++i)
    {
        input_in >> h_maxpool_input[i];
        a_in >> h_a[i];
        b_in >> h_b[i];
        c_in >> h_c[i];
    }
       
    // set thread, block dimensions
    const dim3 block_size(TILE_WIDTH, TILE_WIDTH);
    const dim3 num_of_maxpool_blocks(maxpool_output_size/block_size.x+1, maxpool_output_size/block_size.y+1);
    const dim3 num_of_blocks(input_size/block_size.x+1, input_size/block_size.y+1);

    // memory allocation for the device arrays (array used in GPU)
    float *d_a, *d_b, *d_c, *d_input, *d_gemm_output, *d_maxpool_output;
    cudaMalloc(&d_a, sizeof(float) * input_size * input_size);
    cudaMalloc(&d_b, sizeof(float) * input_size * input_size);
    cudaMalloc(&d_c, sizeof(float) * input_size * input_size);
    cudaMalloc(&d_gemm_output, sizeof(float) * input_size * input_size);
    cudaMalloc(&d_input, sizeof(float) * input_size * input_size);
    cudaMalloc(&d_maxpool_output, sizeof(float) * maxpool_output_size * maxpool_output_size);
    
    // copy host arrays to device array (so can be used in GPU CUDA kernel)
    cudaMemcpy(d_a, h_a, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, h_b, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(d_c, h_c, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(d_input, h_maxpool_input, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);//check

    // launch CUDA kernels

    // First launch gemm kernel using GPU arrays
    gemm<<<num_of_blocks, block_size>>>(d_a, d_b, d_c, alpha, beta, d_gemm_output, input_size);
    cudaDeviceSynchronize();
    cudaError_t error = cudaGetLastError();//check
    if(error!=cudaSuccess)
    {
        fprintf(stderr, "ERROR %s\n", cudaGetErrorString(error));
        return 1;
    }
 
    // Then run maxpooling //check
    maxpool<<<num_of_maxpool_blocks, block_size>>>(d_input, d_maxpool_output, input_size, filter_size);
    cudaDeviceSynchronize();
    error = cudaGetLastError();
    if(error!=cudaSuccess)
    {
        fprintf(stderr, "ERROR %s\n", cudaGetErrorString(error));
        return 1;
    }
 
    // allocate output array in host (so host can catch the results from GPU kernel)
    float *h_gemm_output = (float*) malloc (sizeof(float)*input_size*input_size);
    float *h_maxpool_output = (float*) malloc (sizeof(float)*maxpool_output_size*maxpool_output_size);
    
    // copy results from device to host (pass the result from GPU to host)
    cudaMemcpy(h_gemm_output, d_gemm_output, sizeof(float)*input_size*input_size, cudaMemcpyDeviceToHost);
    cudaMemcpy(h_maxpool_output, d_maxpool_output, sizeof(float)*maxpool_output_size*maxpool_output_size, cudaMemcpyDeviceToHost);
    
    // prints the results
    cout<<"\n========== GEMM OUTPUT ==========\n";
    for (int i = 0; i < input_size * input_size; i++)
    {
        if(i%input_size==0) cout<<"\n";
        cout<<h_gemm_output[i]<<" ";
    }
    cout<<"\n========== MAXPOOL OUTPUT ==========\n";
    for (int i = 0; i < maxpool_output_size * maxpool_output_size; i++)
    {
        if(i%maxpool_output_size==0) cout<<"\n";
        cout<<h_maxpool_output[i]<<" ";
    }
    cout<<'\n';

    //free everything
    cudaFree(d_a);
    cudaFree(d_b);
    cudaFree(d_c);
    cudaFree(d_gemm_output);
    cudaFree(d_input);
    cudaFree(d_maxpool_output);
    free(h_gemm_output);
    free(h_maxpool_output);

    return 0;
}