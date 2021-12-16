def read_inputs(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return [ format(int(h, 16), 'b') for h in lines ]

def version(bits):
    return int(bits[0:3], 2)

def type_id(bits):
    return int(bits[3:6], 2)

def is_literal_packet(bits):
    return type_id(bits) == 4

def is_operator_packet(bits):
    return not is_literal_packet(bits)

def length_type_id(bits):
    return int(bits[6:7], 2)

def literal_packet_groups(bits):
    i = 6
    while i < len(bits):
        last_group = bits[i] == '0'
        yield bits[i+1:i+5]
        if last_group:
            break
        i = i + 5

# unlike most others, spbits starts at the start of the sub-packets
def parse_sub_packets(spbits, num_subpackets=None, debug=False):
    i = 0
    np = 0
    while i < len(spbits) and (not num_subpackets or np < num_subpackets):
        sp = Packet(spbits[i:], debug=debug)
        if sp.terminal:
            return
        i = i + sp.length
        np = np + 1
        yield sp

class Packet:
    def __init__(self, bits, debug=False):
        if len(bits) < 6:
            self.terminal = True
            return
        self.terminal    = False
        self.version     = version(bits)
        self.type_id     = type_id(bits)
        self.is_operator = is_operator_packet(bits)
        self.is_literal  = is_literal_packet(bits)
        if self.is_literal:
            self.groups       = list(literal_packet_groups(bits))
            self.literal      = int(''.join(literal_packet_groups(bits)), 2)
            self.subpackets   = []
            self.length       = 6 + 5 * len(self.groups)
        else:
            self.groups       = []
            self.literal      = None
            if length_type_id(bits) == 0: # total subpacket length
                self.lenlen    = 15
                self.subpacket_length = int(bits[7:7+self.lenlen], 2)
                if debug:
                    print(f'{bits}\nVVVTTTI{"L"*self.lenlen}{"P"*self.subpacket_length}')
                to_parse = bits[7+self.lenlen:7+self.lenlen+self.subpacket_length]
                self.subpackets       = list(parse_sub_packets(to_parse, debug=debug))
            else: # number of subpackets
                self.lenlen    = 11
                if debug:
                    print(f'{bits}\nVVVTTTI{"L"*self.lenlen}...')
                num_subpackets = int(bits[7:7+self.lenlen], 2)
                to_parse = bits[7+self.lenlen:]
                self.subpackets       = list(parse_sub_packets(to_parse, debug=debug, num_subpackets=num_subpackets))
                self.subpacket_length = sum(p.length for p in self.subpackets)
            self.length       = 7 + self.lenlen + self.subpacket_length

    def version_sum(self):
        return self.version + sum(p.version_sum() for p in self.subpackets)

    def __str__(self):
        if self.is_operator:
            return f'V{self.version} OP {self.subpackets}'
        else:
            return f'V{self.version} LIT {self.literal}'

    def __repr__(self):
        return str(self)

def part1(filename):
    return [Packet(p).version_sum() for p in read_inputs(filename)]

#print('\n'.join([f'{Packet(p).version_sum():4} {Packet(p)}' for p in read_inputs('samples.txt')]))
Packet(read_inputs('samples.txt')[1], debug=True)
