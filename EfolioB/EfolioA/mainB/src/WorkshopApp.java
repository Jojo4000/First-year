// Importação de classes Java para I/O, coleções, interfaces funcionais e API JPL7 para Prolog
import java.io.*;
import java.util.*;
import java.util.function.*;
import org.jpl7.*;    

// Classe principal da aplicação
public class WorkshopApp {
    // Diretório onde o projeto OCaml está localizado 
    private static final File OCAML_DIR = new File("..", "mainA");
    //Caminho para o ficheiro database.pl 
    private static final File PROLOG_DB = new File(OCAML_DIR, "_build/default/database.pl");
    // Diretório onde os ficheiros de origem Prolog estão armazenados
    private static final File PROLOG_DIR = new File("src");

    // recebe argumentos da linha de comando
    public static void main(String[] args) throws Exception {
        // exibe instruções de uso e encerra
        if (args.length < 1) {
            usageAndExit();
        }
        // Primeiro argumento define o comando 
        String cmd = args[0];
        // segundo argumento para comandos que necessitam
        String idArg = args.length > 1 ? args[1] : "";

        // Estrutura de seleção pela ordem de trabalhos
        switch (cmd) {
            // Comandos OCaml
            case "listar_items":
            case "orcamento_items":
            case "orcamento_mecanico":
            case "orcamento_desconto_items":
            case "orcamento_preco_fixo":
                // Inovca o OCaml
                invokeOcaml(cmd, idArg);
                break;
            
            //clientes e orçamentos (Questão 1)
            case "listar_clientes":
            case "listar_orcamentos":
                // Encaminha para o handler de lógica 
                handleLogic(cmd);
                break;
            
            //inventário (Questão 2)
            case "listar_itens":
            case "adicionar_item":
            case "remover_item":
            case "atualizar_item":
                // Encaminha para o handler de inventário 
                handleInventory(cmd, idArg);
                break;

            // categorias e descontos (Questão 3)
            case "listar_categorias":
                // Executa o predicado listar_categorias 
                withProlog("categ_disc.pl", () -> 
                {
                    new Query("listar_categorias").hasSolution();
                    return null;
                });
                break;
            case "editar_categoria":
                withProlog("categ_disc.pl", () -> 
                {
                    // Divide argumento em categoria antiga e nova
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    // Cria 'termos' para nomes
                    Term old = new Atom(t[0].trim());
                    Term nw  = new Atom(t[1].trim());
                    // Executa opredicado editar_categoria/2
                    if (new Query("editar_categoria", new Term[]{old, nw}).hasSolution()) 
                    {
                        System.out.println("Categoria editada.");
                    }
                    return null;
                });
                break;
            case "remover_categoria":
                withProlog("categ_disc.pl", () -> 
                {
                    // Cria termo Prolog para categoria a remover
                    Term c = new Atom(idArg.trim());
                    if (new Query("remover_categoria", new Term[]{c}).hasSolution()) 
                    {
                        System.out.println("Categoria removida.");
                    }
                    return null;
                });
                break;
            case "listar_desconto_marca":
                withProlog("categ_disc.pl", () -> 
                {
                    // Executa predicado listar_desconto_marca
                    new Query("listar_desconto_marca").hasSolution();
                    return null;
                });
                break;
            case "adicionar_desconto_marca":
                withProlog("categ_disc.pl", () -> 
                {
                    // Divide argumento em marca e valor de desconto
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    Term b = new Atom(t[0].trim());
                    Term f = new org.jpl7.Float(Double.parseDouble(t[1].trim()));
                    // Executa predicado adicionar_desconto_marca
                    if (new Query("adicionar_desconto_marca", new Term[]{b, f}).hasSolution()) {
                        System.out.println("Desconto de marca adicionado.");
                    }
                    return null;
                });
                break;
            case "editar_desconto_marca":
                withProlog("categ_disc.pl", () -> 
                {
                    // Divide argumentos em marca e novo valor
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    Term b = new Atom(t[0].trim());
                    Term f = new org.jpl7.Float(Double.parseDouble(t[1].trim()));
                    //xecuta predicado editar_desconto_marca/2
                    if (new Query("editar_desconto_marca", new Term[]{b, f}).hasSolution()) 
                    {
                        System.out.println("Desconto de marca editado.");
                    }
                    return null;
                });
                break;
            case "remover_desconto_marca":
                withProlog("categ_disc.pl", () -> 
                {
                    //Cria termo para marca a remover desconto
                    Term b = new Atom(idArg.trim());
                    if (new Query("remover_desconto_marca", new Term[]{b}).hasSolution()) {
                        System.out.println("Desconto de marca removido.");
                    }
                    return null;
                });
                break;
            case "listar_desconto_mao_obra":
                withProlog("categ_disc.pl", () -> 
                {
                    // Executa predicado listar_desconto_mao_obra
                    new Query("listar_desconto_mao_obra").hasSolution();
                    return null;
                });
                break;

            case "adicionar_desconto_mao_obra":
                withProlog("categ_disc.pl", () -> 
                {
                    // Divide argumento em condição e valor
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    Term cond = Term.textToTerm(t[0].trim());
                    Term f    = new org.jpl7.Float(Double.parseDouble(t[1].trim()));
                    // Executa predicado adicionar_desconto_mao_obra/2
                    if (new Query("adicionar_desconto_mao_obra", new Term[]{cond, f}).hasSolution()) 
                    {
                        System.out.println("Desconto de mão de obra adicionado.");
                    }
                    return null;
                });
                break;


            case "editar_desconto_mao_obra":
                withProlog("categ_disc.pl", () -> 
                {
                    // Divide argumento em condição e novo valor
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    Term cond = Term.textToTerm(t[0].trim());
                    Term f    = new org.jpl7.Float(Double.parseDouble(t[1].trim()));
                    // Executa predicado editar_desconto_mao_obra/2
                    if (new Query("editar_desconto_mao_obra", new Term[]{cond, f}).hasSolution()) 
                    {
                        System.out.println("Desconto de mão de obra editado.");
                    }
                    return null;
                });
                break;

            case "remover_desconto_mao_obra":
                withProlog("categ_disc.pl", () -> 
                {
                    // Termo com condição a remover
                    Term cond = Term.textToTerm(idArg.trim());
                    if (new Query("remover_desconto_mao_obra", new Term[]{cond}).hasSolution()) 
                    {
                        System.out.println("Desconto de mão de obra removido.");
                    }
                    return null;
                });
                break;

            // gestão de clientes (Questão 4)
            case "listar_clientes_bd":
                withProlog("cliente.pl", () -> 
                {
                    // Executa o  predicado listar_clientes
                    new Query("listar_clientes").hasSolution();
                    return null;
                });
                break;



            case "adicionar_cliente":
                withProlog("cliente.pl", () -> 
                {
                    // Divide argumento em ID, nome e distrito
                    String[] t = idArg.split(",", 3);
                    if (t.length != 3) usageAndExit();
                    // Cria termos para ID, nome e distrito
                    Term id   = new org.jpl7.Integer(java.lang.Integer.parseInt(t[0].trim()));
                    Term nome = new Atom(t[1].trim());
                    Term dist = new Atom(t[2].trim());
                    // Executa predicado adicionar_cliente/3
                    if (new Query("adicionar_cliente", new Term[]{id, nome, dist}).hasSolution()) 
                    {
                        System.out.println("Cliente adicionado.");
                    }
                    return null;
                });
                break;
            case "remover_cliente":
                withProlog("cliente.pl", () -> {
                    // Cria termo para ID do cliente a remover
                    Term id = new org.jpl7.Integer(java.lang.Integer.parseInt(idArg.trim()));
                    if (new Query("remover_cliente", new Term[]{id}).hasSolution()) 
                    {
                        System.out.println("Cliente removido.");
                    }
                    return null;
                });
                break;
            case "editar_cliente":
                withProlog("cliente.pl", () -> 
                {
                    // Divide argumento em ID, campo e novo valor
                    String[] t = idArg.split(",", 3);
                    if (t.length != 3) usageAndExit();
                    Term id    = new org.jpl7.Integer(java.lang.Integer.parseInt(t[0].trim()));
                    Term campo = Term.textToTerm(t[1].trim());
                    Term val   = new Atom(t[2].trim());
                    // Executa predicado editar_client
                    if (new Query("editar_cliente", new Term[]{id, campo, val}).hasSolution()) 
                    {
                        System.out.println("Cliente editado.");
                    }
                    return null;
                });
                break;
            case "pesquisar_cliente_distrito":
                withProlog("cliente.pl", () -> 
                {
                    // Cria termopara distrito a pesquisar
                    Term dist = new Atom(idArg.trim());
                    new Query("pesquisar_cliente_distrito", new Term[]{dist}).hasSolution();
                    return null;
                });

                break;

            case "pesquisar_cliente_orc_superior":
                withProlog("cliente.pl", () -> 
                {
                    // Divide argumento em valor mínimo e distrito
                    String[] t = idArg.split(",", 2);
                    if (t.length != 2) usageAndExit();
                    Term val  = new org.jpl7.Float(Double.parseDouble(t[0].trim()));
                    Term dist = new Atom(t[1].trim());
                    new Query("pesquisar_cliente_orc_superior", new Term[]{val, dist}).hasSolution();
                    return null;
                });
                break;

            // em casode comando inválido
            default:
                usageAndExit();
        }
    }

    // Handler para inventário: consulta  o Prolog e executa queries do inventário
    private static void handleInventory(String cmd, String idArg) 
    {
        // Inicializa a máquina Prolog via JPL
        JPL.init();                           
        consult(new File(PROLOG_DIR, "inventory.pl"));
        consult(PROLOG_DB);
        switch (cmd) {
            case "listar_itens":
                // Executa listar_itens
                new Query("listar_itens").hasSolution(); 
                break;
            case "adicionar_item": {
                // Divide argumentos e valida o formato
                String[] t = idArg.split(",", 7);
                if (t.length != 7) usageAndExit();
                // Cria  um array de termos para adicionar_item
                Term[] p = 
                {
                    new org.jpl7.Integer(java.lang.Integer.parseInt(t[0].trim())),
                    new Atom(t[1].trim()),
                    new Atom(t[2].trim()),
                    new Atom(t[3].trim()),
                    new org.jpl7.Float(Double.parseDouble(t[4].trim())),
                    new org.jpl7.Float(Double.parseDouble(t[5].trim())),
                    new org.jpl7.Integer(java.lang.Integer.parseInt(t[6].trim()))
                };
                if (new Query("adicionar_item", p).hasSolution()) 
                {
                    System.out.println("Item adicionado.");
                }
            } 
            break;
            case "remover_item": 
            {
                // Cria array com o ID para remover_item/1
                Term[] p = { new org.jpl7.Integer(java.lang.Integer.parseInt(idArg.trim())) };
                if (new Query("remover_item", p).hasSolution()) 
                {
                    System.out.println("Item removido.");
                }
            } break;

            case "atualizar_item": 
            {
                // Divide argumentos: ID, campo e novo valor
                String[] t = idArg.split(",", 3);
                if (t.length != 3) usageAndExit();
                Term id    = new org.jpl7.Integer(java.lang.Integer.parseInt(t[0].trim()));
                Term campo = Term.textToTerm(t[1].trim());
                Term val;  // Determina tipo do valor (float, int ou atom)
                switch (campo.arg(1).name()) 
                {
                    case "custo":
                    case "preco":   val = new org.jpl7.Float(Double.parseDouble(t[2].trim())); break;
                    case "quantidade": val = new org.jpl7.Integer(java.lang.Integer.parseInt(t[2].trim())); break;
                    default:          val = new Atom(t[2].trim());
                }
                Term[] p = {id, campo, val};
                if (new Query("atualizar_item", p).hasSolution()) 
                {
                    System.out.println("Item atualizado.");
                }
            } 
            break;
        }
    }

    // Handler para lógica: consulta Logic.pl e executa listar_clientes/listar_orcamentos
    private static void handleLogic(String cmd) 
    {
        JPL.init();
        consult(new File(PROLOG_DIR, "Logic.pl"));
        consult(PROLOG_DB);
        Variable L = new Variable("L"); // Variável Prolog para receber lista
        if (cmd.equals("listar_clientes")) 
        {
            Query q = new Query("listar_clientes", new Term[]{L});
            if (q.hasSolution()) 
            {
                for (Term t : q.oneSolution().get("L").listToTermArray()) 
                {
                    System.out.printf("%d: %s (%s)%n", t.arg(1).intValue(), t.arg(2).name(), t.arg(3).name());
                }
            }
        } else {
            Query q = new Query("listar_orcamentos", new Term[]{L});
            if (q.hasSolution()) 
            {

                for (Term o : q.oneSolution().get("L").listToTermArray()) 
                {
                    System.out.println(o);
                }
            }
        }
    }

    // Helper que consulta um ficheiro Prolog e depois o database.pl
    private static <T> T withProlog(String sourceFile, Supplier<T> action) {
        consult(new File(PROLOG_DIR, sourceFile));
        consult(PROLOG_DB);
        // Executa a lambda fornecida
        return action.get(); 
    }

    // Helper que efetua consult em um único ficheiro Prolog e encerra se falhar
    private static void consult(File f) {
        if (!new Query("consult", new Term[]{new Atom(f.getAbsolutePath())}).hasSolution()) 
        {
            System.err.println("ERRO: não foi possível consultar " + f);
            System.exit(1);
        }
    }

    // Invocador OCaml: constrói ProcessBuilder para "dune exec" e processa saída
    private static void invokeOcaml(String cmd, String ids) throws IOException, InterruptedException 
    {
        if (cmd.equals("listar_items")) 
        {
            // Chama runAndPrint para imprimir cada linha da saída OCaml
            runAndPrint(cmd, ids, System.out::println);
            return;
        }
        Function<String,Object> mapper;
        switch (cmd) 
        {
            case "orcamento_items":           mapper = Part::fromLine; break;
            case "orcamento_mecanico":        mapper = LaborCost::fromLine; break;
            case "orcamento_desconto_items":  mapper = PartDiscount::fromLine; break;
            case "orcamento_preco_fixo":      mapper = WorkshopApp::fixedPriceFromLine; break;
            default: return;
        }
        for (Object o : runAndParse(cmd, ids, mapper)) 
        {
            // Imprime os objetos mapeados: Part, LaborCost, PartDiscount ou double[]
            if (o instanceof Part p) 
            {
                System.out.printf("%d → %s @ lucro %.2f%n", p.serviceId, p.name, p.lucro);
            } else if (o instanceof LaborCost lc) 
            {
                System.out.printf("%d → horas=%.2f total=%.2f%n", lc.serviceId, lc.horas, lc.total);
            } else if (o instanceof PartDiscount pd)
            {
                System.out.printf("%d → pct=%.2f%% valor=%.2f%n", pd.getItemId(), pd.getPct(), pd.getValue());
            } else if (o instanceof double[] fp) 
            {
                System.out.printf("%d → preco_fixo=%.2f%n", (int) fp[0], fp[1]);
            }
        }
    }

    // Executa comando OCaml e envia cada linha para um Consumer<String>
    private static void runAndPrint(String cmd, String ids, Consumer<String> c) throws IOException, InterruptedException 
    {
        ProcessBuilder pb = new ProcessBuilder(buildCommand(cmd, ids)).directory(OCAML_DIR).redirectErrorStream(true); // Redireciona stderr para stdout
        Process p = pb.start();
        try (BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()))) 
        {
            String line;
            while ((line = r.readLine()) != null) 
            {
                c.accept(line);
            }
        }
        p.waitFor();
    }

    // Executa comando OCaml, mapeia linhas para objetos e retorna lista
    private static <T> List<T> runAndParse(String cmd, String ids, Function<String,T> mapper) throws IOException, InterruptedException 
    {
        List<T> out = new ArrayList<>();
        ProcessBuilder pb = new ProcessBuilder(buildCommand(cmd, ids)).directory(OCAML_DIR).redirectErrorStream(true);
        Process p = pb.start();
        try (BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()))) 
        {
            String l;
            while ((l = r.readLine()) != null) 
            {
                l = l.trim();
                // Filtra apenas as linhas que começam com dígitos
                if (!l.isEmpty() && Character.isDigit(l.charAt(0))) 
                {
                    out.add(mapper.apply(l));
                }
            }
        }

        p.waitFor();
        return out;
    }

    // Constrói lista de Strings para o comando ProcessBuilder 
    private static List<String> buildCommand(String cmd, String ids) 
    {
        List<String> c = new ArrayList<>(List.of("dune", "exec", "mainA", "--", cmd));
        if (!ids.isEmpty()) c.add(ids);
        return c;
    }

    // Interpreta saída fixPrice do OCaml
    private static double[] fixedPriceFromLine(String ln) 
    {
        String[] t = ln.split(";");
        return new double[]{Double.parseDouble(t[0]), Double.parseDouble(t[1])};
    }

    // Exibe as instruções para bom uso no stderr e encerra com código com erro
    private static void usageAndExit() 
    {
        System.err.println("Uso:");
        System.err.println("  WorkshopApp listar_items");
        System.err.println("  WorkshopApp orcamento_items <ids>");
        System.err.println("  WorkshopApp orcamento_mecanico <ids>");
        System.err.println("  WorkshopApp orcamento_desconto_items <ids>");
        System.err.println("  WorkshopApp orcamento_preco_fixo <ids>");
        System.err.println("  WorkshopApp listar_itens");
        System.err.println("  WorkshopApp adicionar_item <ID>,<Nome>,<Marca>,<Tipo>,<Custo>,<Venda>,<Qtd>");
        System.err.println("  WorkshopApp remover_item <ID>");
        System.err.println("  WorkshopApp atualizar_item <ID>,campo(<campo>),<Novo>");
        System.err.println("  WorkshopApp listar_clientes");
        System.err.println("  WorkshopApp listar_orcamentos");
        System.err.println("  WorkshopApp listar_categorias");
        System.err.println("  WorkshopApp editar_categoria <Antiga>,<Nova>");
        System.err.println("  WorkshopApp remover_categoria <Categoria>");
        System.err.println("  WorkshopApp listar_desconto_marca");
        System.err.println("  WorkshopApp adicionar_desconto_marca <Marca>,<Valor>");
        System.err.println("  WorkshopApp editar_desconto_marca <Marca>,<Valor>");
        System.err.println("  WorkshopApp remover_desconto_marca <Marca>");
        System.err.println("  WorkshopApp listar_desconto_mao_obra");
        System.err.println("  WorkshopApp adicionar_desconto_mao_obra <Cond>,<Valor>");
        System.err.println("  WorkshopApp editar_desconto_mao_obra <Cond>,<Valor>");
        System.err.println("  WorkshopApp remover_desconto_mao_obra <Cond>");
        System.err.println("  WorkshopApp listar_clientes_bd");
        System.err.println("  WorkshopApp adicionar_cliente <ID>,<Nome>,<Distrito>");
        System.err.println("  WorkshopApp remover_cliente <ID>");
        System.err.println("  WorkshopApp editar_cliente <ID>,campo(<campo>),<Novo>");
        System.err.println("  WorkshopApp pesquisar_cliente_distrito <Distrito>");
        System.err.println("  WorkshopApp pesquisar_cliente_orc_superior <Valor>,<Distrito>");
        System.exit(1);
    }
}
