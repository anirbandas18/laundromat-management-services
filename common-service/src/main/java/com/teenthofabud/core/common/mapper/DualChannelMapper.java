package com.teenthofabud.core.common.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;

import java.util.Optional;

@FunctionalInterface
public interface DualChannelMapper<T, S>{

    public Optional<T> compareAndMap(T reference, S source) throws TOABBaseException;

}
