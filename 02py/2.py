from dataclasses import dataclass


@dataclass
class Submarine:
    
    depth: int = 0
    position: int = 0

    def down(self, units):
        self.depth += units

    def up(self, units):
        self.depth -= units

    def forward(self, units):
        self.position += units


@dataclass
class Submarine2:
    
    depth: int = 0
    position: int = 0
    aim: int = 0

    def down(self, units):
        self.aim += units

    def up(self, units):
        self.aim -= units

    def forward(self, units):
        self.position += units
        self.depth += units * self.aim


def get_dive_factor(sub_type, directions):
    sub = sub_type()
    for direction, units in map(lambda x: x.split(), directions):
        getattr(sub, direction)(int(units))
    return sub.depth * sub.position


if __name__ == "__main__":
    with open("2.ex.txt", "r") as f:
        directions = f.read().split("\n")
    dive_factor = get_dive_factor(Submarine, directions)
    assert dive_factor == 150

    with open("2.in.txt", "r") as f:
        directions = f.read().split("\n")
    print(get_dive_factor(Submarine, directions))

    with open("2.ex.txt", "r") as f:
        directions = f.read().split("\n")
    dive_factor = get_dive_factor(Submarine2, directions)
    assert dive_factor == 900

    with open("2.in.txt", "r") as f:
        directions = f.read().split("\n")
    print(get_dive_factor(Submarine2, directions))
