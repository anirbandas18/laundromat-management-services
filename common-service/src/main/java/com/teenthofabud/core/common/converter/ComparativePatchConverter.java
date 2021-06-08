package com.teenthofabud.core.common.converter;

import com.teenthofabud.core.common.data.error.TOABBaseException;

@FunctionalInterface
public interface ComparativePatchConverter<T, S>{

    public void compareAndMap(T reference, S source) throws TOABBaseException;

}
