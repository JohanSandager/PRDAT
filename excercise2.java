public class Main {
    public static void main(String[] args) {
        Expr e = new Add(new CstI(17), new Var("z"));
        System.out.println(e.toString()); 
    }
}
abstract class Expr { }

class CstI extends Expr {
    protected final int i;

    public CstI(int i) {
    this.i = i;
    }

    @Override
    public String toString() { 
        return "Csti " + Integer.toString(i);
    } 
}
class Var extends Expr {
    protected final String name;

    public Var(String name) {
    this.name = name;
    }

    @Override
    public String toString() { 
        return "Var " + name;
    } 
}

abstract class Binop extends Expr { 
        public Binop(Expr e1, Expr e2) { 
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add extends Binop {
    public Add(Expr e1, e2) {
        super(e1, e2)
    }
    
    @Override
    public String toString() { 
    return "Add(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}

class Mul extends Binop {
    public Mul(Expr e1, e2) {
        super(e1, e2)
    }
    
    @Override
    public String toString() { 
    return "Mul(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}

class Sub extends Binop {
    public Mul(Expr e1, e2) {
        super(e1, e2)
    }
    
    @Override
    public String toString() { 
    return "Sub(" + e1.toString() + ", " + e2.toString() + ")";
    } 
}
