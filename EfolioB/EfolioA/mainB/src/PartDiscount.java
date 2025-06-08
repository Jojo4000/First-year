public class PartDiscount {
  private final int    itemId;
  private final double pct, value;

  public PartDiscount(int itemId, double pct, double value) {
    this.itemId = itemId;
    this.pct    = pct;
    this.value  = value;
  }

  /** Parse a line like
   *   1;10.00;2.60
   */
  public static PartDiscount fromLine(String line) {
    String[] t = line.split(";");
    return new PartDiscount(
      Integer.parseInt(t[0]),
      Double.parseDouble(t[1]),
      Double.parseDouble(t[2])
    );
  }

  public int    getItemId() { return itemId; }
  public double getPct()    { return pct; }
  public double getValue()  { return value; }
}
