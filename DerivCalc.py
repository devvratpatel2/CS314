from Tkinter import *
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib
from matplotlib.figure import Figure
from numpy import arange
from ExpPar import *
import tkMessageBox
import webbrowser

class Grapher:
    
    def __init__(self, master):

        self.topFrame = Frame(master,relief=RAISED, borderwidth=1,width=400,height=300)
        self.topFrame.pack(fill=BOTH)
        self.bottomFrame = Frame(master,relief=RAISED, borderwidth=1,width=400,height=200)
        self.bottomFrame.pack(fill=BOTH)
        
        self.inputLabel = Label(self.bottomFrame,text="f(x)=")
        self.inputLabel.place(relx=0.18, rely=0.3, anchor=CENTER)
        
        self.input = Entry(self.bottomFrame,width=30)
        self.input.place(relx=0.54, rely=0.3, anchor=CENTER)
        
        self.outputLabel = Label(self.bottomFrame,text="f'(x)=")
        self.outputLabel.place(relx=0.18, rely=0.45, anchor=CENTER)
        
        self.outputText = StringVar()
        
        self.output = Entry(self.bottomFrame,width=30,text=self.outputText)
        self.output.config(state='readonly')
        self.output.place(relx=0.54, rely=0.45,anchor=CENTER)
        
        self.computeButton = Button(self.bottomFrame,text="Compute",command=self.compute)
        self.computeButton.place(relx=0.5,rely=0.6,anchor=CENTER)       
        
        self.infoButton = Button(self.bottomFrame,text="info",command=self.info)
        self.infoButton.place(relx=0.5,rely=0.75,anchor=CENTER)       
        
        self.plotBoard = Figure(figsize=(5, 3), dpi=100)

        self.canvas = FigureCanvasTkAgg(self.plotBoard, master=self.topFrame)
        self.canvas.show()
        self.canvas.get_tk_widget().pack(side=TOP)
        

        
    def compute(self):
        input = self.input.get()
        try:
            p = Parser(input,'x')
            f = p.parse()
            fprime = f.derivative()
            self.outputText.set(str(fprime))
            xs = arange(-25.0,25.0,0.5)
            validXs,validYs = self.safeCompute(xs, f.compute)
            validXPs,validYPs = self.safeCompute(xs,fprime.compute)
            self.plotBoard.clear()
            subplot = self.plotBoard.add_subplot(111)
            subplot.plot(validXs,validYs,label="f(x)")
            subplot.plot(validXPs,validYPs,label="f '(x)")
            subplot.legend(loc='upper center', shadow=True, fontsize='x-small')
            self.canvas.show()
        except (ParsingError,ParserError):
            tkMessageBox.showerror("Parsing Error","Remaining string: " + str(p) + "\nMake sure to use ( ) around functions\n e.g. [no]sin x [ok]sin(x)")            
            
    def safeCompute(self,xs,f):
        validXs = []
        validYs = []
        for x in xs:
            try:
                validYs.append(f(x))
                validXs.append(x)
            except (ValueError,ZeroDivisionError):
                pass
        return (validXs,validYs)
            
def center(win):
    win.update_idletasks()
    width = win.winfo_width()
    height = win.winfo_height()
    x = (win.winfo_screenwidth() // 2) - (width // 2)
    y = (win.winfo_screenheight() // 2) - (height // 2)
    win.geometry('{}x{}+{}+{}'.format(width, height, x, y))    
    
def main():
    root = Tk()
    root.geometry("400x500+500+200")
    root.wm_title("Derivative Grapher")
    root.resizable(0,0)
    grapher = Grapher(root)
    center(root)
    root.lift()
    root.mainloop()

if __name__ == "__main__":
    main()