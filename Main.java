
public class Main {
  public static void main(String[] args) {
    var a = new dotty.tools.scaladoc.Main();
    var r = a.run(args);
    System.out.println("" + r.hasErrors());
  }
}
