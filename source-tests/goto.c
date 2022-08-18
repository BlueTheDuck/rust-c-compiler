void main() {
    int x = 0x10;
    goto label;
    x = 0x01;
back:
    x = x + 1;
}

void test() {
    int x;
label:
    x = 0xF0;
    goto back;
}
