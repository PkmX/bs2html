#include <cstdint>
#include <fstream>

struct hdr_t {
  int32_t  chrono;
  uint32_t xmode;
  int32_t  xid; // unused
  int8_t   xname[32];
  int8_t   owner[80];
  int8_t   nick[49];
  int8_t   score;
  int8_t   date[9];
  int8_t   title[73];
};

static_assert(sizeof(hdr_t) == 256);

int main() {
    const hdr_t hdrs[] = { { 0x12345678, 0x00000800 /* POST_RESTRICT */, 0, "A1234567", "testowner", "Test Nick", 0, "16/07/27", "[\xb4\xfa\xb8\xd5]" /* [測試] */ }
                         , { 0x12345679, 0, 0, "ABCDEFGH", "PkmX", "\xaa\xfc\xbf\xdf" /* 阿貓 */, 0, "16/07/27", "[\xac\xec\xac\xec]" /* [科科] */ }
                         };

    std::ofstream out(".DIR");
    out.exceptions(std::ostream::failbit);
    for (auto&& hdr: hdrs) {
        out.write(reinterpret_cast<const char*>(&hdr), sizeof(hdr_t));
    }

    return 0;
}
