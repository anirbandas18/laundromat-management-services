package com.teenthofabud.core.common.converter;

import java.util.Optional;

@FunctionalInterface
public interface ComparativeFormConverter<T, S>{

    public Optional<T> compareAndMap(T reference, S source);

}
