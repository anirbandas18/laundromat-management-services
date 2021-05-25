package com.teenthofabud.core.common.mapper;

import java.util.Optional;

@FunctionalInterface
public interface SingleChannelMapper<T>{

    public Optional<T> compareAndMap(T reference, T source);

}
