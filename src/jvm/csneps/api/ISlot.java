package csneps.api;

public interface ISlot {
    Long getMax();
    Long getMin();
    String getName();
    ISemanticType getType();
}
