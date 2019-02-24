import queue

class Pos:
    def __init__(self,name):
        self.name = name
        self.normalForward = []
        self.invertForward = []
        self.normalReverse = []
        self.invertReverse = []
        self.normalRotate = []
        self.invertRotate = []
    def addConnection(self, other, dir = 1, inv = False):
        if (dir == 1) and not inv:
            self.normalForward.append(other)
        elif (dir == 1) and inv:
            self.invertForward.append(other)
        elif (dir == -1) and not inv:
            self.normalReverse.append(other)
        elif (dir == -1) and inv:
            self.invertReverse.append(other)
    def addRotation(self, other, inv = False):
        if inv:
            self.invertRotate.append(other)
        else:
            self.normalRotate.append(other)
    def __repr__(self):
        return self.name

class Gear:
    def __init__(self, corner, dir, pos, history):
        self.corner = corner
        self.dir = dir
        self.pos = pos
        self.history = history
    def getPossiblePos(self):
        NF = [Gear((self.corner + self.dir)%5, self.dir, pos, self.history + [pos.name]) for pos in self.pos.normalForward]
        IF = [Gear((self.corner + self.dir)%5, (-1) *  self.dir, pos, self.history + [pos.name]) for pos in self.pos.invertForward]
        NR = [Gear((self.corner - self.dir)%5, self.dir, pos, self.history + [pos.name]) for pos in self.pos.normalReverse]
        IR = [Gear((self.corner - self.dir)%5, (-1) * self.dir, pos, self.history + [pos.name]) for pos in self.pos.invertReverse]
        R = [Gear(self.corner, self.dir, pos, self.history + [pos.name]) for pos in self.pos.normalRotate]
        RR = [Gear(self.corner, (-1) * self.dir, pos, self.history + [pos.name]) for pos in self.pos.invertRotate]
        return sum([NF, IF, NR, IR, R, RR], [])
    def isWinningState(self):
        return (self.corner == 0) and (self.dir == 1) and (self.pos == top)
    def isWinningStateAlt(self):
        return (self.corner == 1) and (self.dir == -1) and (self.pos == back)
    def checkState(self):
        #print("checking state {name}".format(name=self.pos.name))
        if self.isWinningState():
            print(self.history)
            return True
        return False
    def __repr__(self):
        return self.pos.name

# face direction top, forward, right

top = Pos('top')
bot = Pos('bot')
left = Pos('left')
right = Pos('right')
front = Pos('front')
back = Pos('back')
topR = Pos('topR')
botR = Pos('botR')
leftR = Pos('leftR')
rightR = Pos('rightR')
frontR = Pos('frontR')
backR = Pos('backR')



top.addRotation(topR)
topR.addRotation(top)

bot.addRotation(botR, inv=True)
botR.addRotation(bot, inv=True)

left.addRotation(leftR, inv=True)
leftR.addRotation(left, inv=True)

right.addRotation(rightR)
rightR.addRotation(right)

front.addRotation(frontR)
frontR.addRotation(front)

back.addRotation(backR, inv=True)
backR.addRotation(back, inv=True)


topR.addConnection(right, dir=1, inv=True)
topR.addConnection(left, dir=-1)

bot.addConnection(front, dir=-1, inv=True)
bot.addConnection(back, dir=1)
botR.addConnection(right, dir=1)

left.addConnection(topR, dir=1)
leftR.addConnection(backR, dir=1)
leftR.addConnection(frontR, dir=-1, inv=True)

right.addConnection(topR, dir=1, inv=True)
right.addConnection(botR, dir=-1)
rightR.addConnection(backR, dir=1, inv=True)

front.addConnection(bot, dir=-1, inv=True)
frontR.addConnection(leftR, dir=-1, inv=True)

back.addConnection(bot, dir=-1)
backR.addConnection(leftR, dir=-1)
backR.addConnection(rightR, dir=1, inv=True)



p = Gear(0, 1, front, [])
frontier = queue.Queue()
frontier.put(p)
while not p.checkState():
    #print(frontier.queue)
    for state in p.getPossiblePos():
        frontier.put(state)
        if len(state.history) >= 15:
            raise StandardError("Maximum search depth exceeded")
    p = frontier.get()