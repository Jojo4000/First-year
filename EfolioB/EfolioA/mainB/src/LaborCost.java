public class LaborCost {
  public final int    serviceId;
  public final double horas, costHour, base, discount, total;

  public LaborCost(int serviceId,
                   double horas,
                   double costHour,
                   double base,
                   double discount,
                   double total) {
    this.serviceId = serviceId;
    this.horas     = horas;
    this.costHour  = costHour;
    this.base      = base;
    this.discount  = discount;
    this.total     = total;
  }

  /** Parse a line like
   *   1;0.50;8.00;4.00;0.00;4.00
   */
  public static LaborCost fromLine(String line) {
    String[] t = line.split(";");
    return new LaborCost(
      Integer.parseInt(t[0]),
      Double.parseDouble(t[1]),
      Double.parseDouble(t[2]),
      Double.parseDouble(t[3]),
      Double.parseDouble(t[4]),
      Double.parseDouble(t[5])
    );
  }
}
