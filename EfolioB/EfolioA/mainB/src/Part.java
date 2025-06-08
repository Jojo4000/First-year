public class Part {
  public final int id;
  public final int serviceId;
  public final String name;
  public final String brand;
  public final String category;
  public final double cost;
  public final double price;
  public final double discountFrac;
  public final double lucro;

  public Part(int id, int serviceId, String name, String brand, String category,
              double cost, double price, double discountFrac, double lucro) {
    this.id           = id;
    this.serviceId    = serviceId;
    this.name         = name;
    this.brand        = brand;
    this.category     = category;
    this.cost         = cost;
    this.price        = price;
    this.discountFrac = discountFrac;
    this.lucro        = lucro;
  }

  /** Parse a line like
   *   8;1;Oleo Edge 5W30;Castrol;Oleos;35.00;45.99;0.00;10.99
   */
  public static Part fromLine(String line) {
    String[] t = line.split(";");
    return new Part(
      Integer.parseInt(t[0]),
      Integer.parseInt(t[1]),
      t[2], t[3], t[4],
      Double.parseDouble(t[5]),
      Double.parseDouble(t[6]),
      Double.parseDouble(t[7]),
      Double.parseDouble(t[8])
    );
  }
}
