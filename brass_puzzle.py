import queue

class Pos:
    def __init__(self,name):
        self.name = name
        self.normalForward = []
        self.invertForward = []
        self.normalReverse = []
        self.invertReverse = []
        self.rotate = []
        self.rotateReverse = []
    def addNormalForward(self, other):
        self.normalForward.append(other)
    def addInvertForward(self, other):
        self.invertForward.append(other)
    def addNormalReverse(self, other):
        self.normalReverse.append(other)
    def addInvertReverse(self, other):
        self.invertReverse.append(other)
    def setRotate(self, other):
        self.rotate.append(other)
    def setRotateReverse(self, other):
        self.rotateReverse.append(other)
    def __repr__(self):
        return self.name

class Pentagon:
    def __init__(self, corner, dir, pos, history):
        self.corner = corner
        self.dir = dir
        self.pos = pos
        self.history = history
    def getPossiblePos(self):
        NF = [Pentagon((self.corner + self.dir)%5, self.dir, pos, self.history + [pos.name]) for pos in self.pos.normalForward]
        IF = [Pentagon((self.corner + self.dir)%5, (-1) *  self.dir, pos, self.history + [pos.name]) for pos in self.pos.invertForward]
        NR = [Pentagon((self.corner - self.dir)%5, self.dir, pos, self.history + [pos.name]) for pos in self.pos.normalReverse]
        IR = [Pentagon((self.corner - self.dir)%5, (-1) * self.dir, pos, self.history + [pos.name]) for pos in self.pos.invertReverse]
        R = [Pentagon(self.corner, self.dir, pos, self.history + [pos.name]) for pos in self.pos.rotate]
        RR = [Pentagon(self.corner, (-1) * self.dir, pos, self.history + [pos.name]) for pos in self.pos.rotateReverse]
        return sum([NF, IF, NR, IR, R, RR], [])
    def isWinningState(self):
        return (self.corner == 0) and (self.dir == 1) and (self.pos == top)
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

top.setRotate(topR)
topR.setRotate(top)

bot.setRotateReverse(botR)
botR.setRotateReverse(bot)

left.setRotateReverse(leftR)
leftR.setRotateReverse(left)

right.setRotate(rightR)
rightR.setRotate(right)

front.setRotate(frontR)
frontR.setRotate(front)

back.setRotateReverse(backR)
backR.setRotateReverse(back)


topR.addInvertForward(right)
topR.addNormalReverse(left)

bot.addInvertReverse(front)
bot.addNormalForward(back)
botR.addNormalForward(right)

left.addNormalForward(topR)
leftR.addNormalForward(backR)
leftR.addInvertReverse(frontR)

right.addInvertForward(topR)
right.addNormalReverse(botR)
rightR.addInvertForward(backR)

front.addInvertReverse(bot)
frontR.addInvertReverse(leftR)

back.addNormalReverse(bot)
backR.addNormalReverse(leftR)
backR.addInvertForward(rightR)



p = Pentagon(3, 1, back, [])
frontier = queue.Queue()
frontier.put(p)
while not p.checkState():
    #print(frontier.queue)
    for state in p.getPossiblePos():
        frontier.put(state)
        if len(state.history) >= 8:
            raise StandardError("Maximum search depth exceeded")
    p = frontier.get()