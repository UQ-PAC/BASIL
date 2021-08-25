import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BoogieCleaner {

    String inputFileName;
    String outputFileName;
    List<String> usedLabels = new ArrayList<>();


    public BoogieCleaner(String inputFileName, String outputFileName) {
        this.inputFileName = inputFileName;
        this.outputFileName = outputFileName;
    }

    public void clean() {
        String line;
        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFileName));
            while ((line = reader.readLine()) != null) {
                // strip leading and trailing whitespace
                line = line.trim();
                // check if a label is used
                if (line.contains("goto label")) { // warning: labels must start with 'label'
                    int i = line.indexOf("goto label") + 5; // warning: this does not allow multiple gotos on one line
                    char c;
                    StringBuilder label = new StringBuilder();
                    c = line.charAt(i);
                    while (Character.isDigit(c) || Character.isLetter(c)) { // warning: labels generated must only contain letters and numbers
                        label.append(c);
                        c = line.charAt(++i);
                    }
                    usedLabels.add(label.toString());
                }
            }
            for (String label : usedLabels) {
                System.out.printf("used label: %s%n", label);
            }
        } catch (IOException e) {
            System.err.println("Error reading input file.");
        }

        try {
            BufferedReader reader = new BufferedReader(new FileReader(inputFileName));
            BufferedWriter writer = new BufferedWriter(new FileWriter(outputFileName));
            while ((line = reader.readLine()) != null) {
                if (line.trim().startsWith("label")) {
                    int i = line.indexOf("label");
                    int start = i;
                    while (line.charAt(i) != ':') {
                        i++;
                    }
                    String label = line.substring(start, i);
                    if (!usedLabels.contains(label)) {
                        // concatenates the whitespace before the label with the rest of the line, ignoring the ": "
                        // after the label
                        line = line.substring(0, start) + line.substring(i + 1).trim(); // warning: cuts off trailing whitespace
                    }
                }
                writer.write(line + "\n");
                writer.flush();
            }
        } catch (IOException e) {
            System.err.println("Error reading or writing input file.");
        }
    }
}
