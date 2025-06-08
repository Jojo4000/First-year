// Importa classes para entrada/saída de ficheiros e erros
import java.io.*;
// Importa estruturas de dados como List e ArrayList
import java.util.*;
// Importa ferramentas de manipulação funcional de listas
import java.util.stream.*;

public class OficinaApp {

  // Executa o binário OCaml com o comando especificado
  public static List<String> runOcaml(String command, List<Integer> ids)
    throws IOException, InterruptedException {
  
  // Prepara IDs para serem passados como argumento
  String argIds = ids.isEmpty() ? "" : ids.stream().map(Object::toString).collect(Collectors.joining(","));

  // Monta o comando para executar o programa OCaml
  List<String> cmd = new ArrayList<>();
  cmd.add("dune");
  cmd.add("exec");
  cmd.add("mainA");
  cmd.add("--");
  cmd.add(command);
  
  // Se houver IDs, adiciona-os ao comando
  if (!argIds.isEmpty()) cmd.add(argIds);

  // Cria o processo para executar o comando
  ProcessBuilder pb = new ProcessBuilder(cmd);
  // Define o diretório de trabalho do processo
  pb.directory(new File("/home/jojo4000/LingPproj/EfolioA/mainA"));
  // Redireciona erros para o mesmo fluxo de saída
  pb.redirectErrorStream(true);

  // Inicia o processo
  Process p = pb.start();
  
  // Lê e devolve as linhas da saída do processo
  try (BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
    List<String> out = reader.lines().toList();
    p.waitFor();
    return out;
  }
}

  /**
   * Representa uma linha do resultado de "orcamento_items"
   */
  public static class Part {
    public final int id, serviceId;
    public final String nome, marca, tipo;
    public final double custo, preco, descontoFrac, lucro;

    // Construtor privado da Part
    private Part(int id,int sid,String nome,String marca,String tipo,double custo,double preco,double d,double l) {
      this.id = id; this.serviceId = sid;
      this.nome = nome; this.marca = marca; this.tipo = tipo;
      this.custo = custo; this.preco = preco;
      this.descontoFrac = d; this.lucro = l;
    }

    // Converte uma linha de texto em um objeto Part
    public static Part fromLine(String line) {
      // Divide a linha em campos separados por ";"
      String[] tok = line.split(";");
      return new Part(
        Integer.parseInt(tok[0]),
        Integer.parseInt(tok[1]),
        tok[2], tok[3], tok[4],
        Double.parseDouble(tok[5]),
        Double.parseDouble(tok[6]),
        Double.parseDouble(tok[7]),
        Double.parseDouble(tok[8])
      );
    }
  }

  /**
   * Representa uma linha do resultado de "orcamento_mecanico"
   */
  public static class LaborCost {
    public final int serviceId;
    public final double horas, custoHora, bruto, desconto, total;

    // Construtor privado da LaborCost
    private LaborCost(int sid,double h,double ch,double b,double d,double t) {
      this.serviceId = sid;
      this.horas = h;
      this.custoHora = ch;
      this.bruto = b;
      this.desconto = d;
      this.total = t;
    }

    // Converte uma linha de texto em um objeto LaborCost
    public static LaborCost fromLine(String line) {
      // Divide a linha em campos separados por ";"
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

  // Método principal que interpreta os comandos recebidos
  public static void main(String[] args) throws IOException, InterruptedException {
    // Verifica se o utilizador passou pelo menos um argumento
    if (args.length < 1) {
      System.err.println("Uso: java OficinaApp <comando> [<id1,id2,...>]");
      System.exit(1);
    }
    
    // Primeiro argumento é o comando
    String cmd = args[0];

    // Segundo argumento são os IDs separados por vírgula, se existirem
    List<Integer> ids = args.length > 1
      ? Arrays.stream(args[1].split(","))
          .map(String::trim)
          .map(Integer::parseInt)
          .toList()
      : Collections.emptyList();

    // Executa o comando apropriado
    switch (cmd) {
      case "listar_items" -> {
        // Lista todos os items diretamente
        for (String line : runOcaml("listar_items", ids)) {
          System.out.println(line);
        }
      }
      case "orcamento_items" -> {
        // Processa a saída em objetos Part e imprime o lucro de cada peça
        List<Part> parts = runOcaml("orcamento_items", ids).stream().map(Part::fromLine).toList();
        double totalLucro = 0.0;
        for (Part p : parts) {
          System.out.println(p.serviceId + " → " + p.nome + " @ lucro " + String.format("%.2f", p.lucro));
          totalLucro += p.lucro;
        }
        System.out.println("Lucro total peças: " + String.format("%.2f", totalLucro));
      }
      case "orcamento_mecanico" -> {
        // Processa a saída em objetos LaborCost e imprime custos de mão de obra
        List<LaborCost> labors = runOcaml("orcamento_mecanico", ids).stream().map(LaborCost::fromLine).toList();
        double totalLabor = 0.0;
        for (LaborCost lc : labors) {
          System.out.println(lc.serviceId + " → horas=" + lc.horas + " custoHora=" + lc.custoHora + " total=" + String.format("%.2f", lc.total));
          totalLabor += lc.total;
        }
        System.out.println("Custo total mão de obra: " + String.format("%.2f", totalLabor));
      }
      default -> {
        // Se o comando não for reconhecido, mostra mensagem de erro
        System.err.println("Comando desconhecido: " + cmd);
        System.err.println("Opções: listar_items | orcamento_items <ids> | orcamento_mecanico <ids>");
        System.exit(1);
      }
    }
  }
}