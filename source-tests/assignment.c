int global;
void main() {
    int local = 0;
    local = 5;
    global = local;
    local = global;
}