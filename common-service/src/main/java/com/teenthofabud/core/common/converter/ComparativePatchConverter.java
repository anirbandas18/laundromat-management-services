package com.teenthofabud.core.common.converter;

@FunctionalInterface
public interface ComparativePatchConverter<T, S>{

    public void compareAndMap(T reference, S source);

}
