#include <systemc.h>
#include <vector>


template<typename T>
SC_MODULE(Mux) {
    sc_vector<sc_in<T>> in;
    sc_in<uint32_t> sel;

    sc_out<T> out;


    Mux(sc_module_name name, size_t n) : sc_module(name), in("in", n) {
        SC_METHOD(execute);
        sensitive << sel;
        for (size_t i = 0; i < n; ++i) {
            sensitive << in[i];
        }
    }


    void execute() {
        uint32_t index = sel.read();
        if (index < in.size()) {
            out.write(in[index].read());
        }
    }
};
