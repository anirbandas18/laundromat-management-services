package com.teenthofabud.core.common.repository;

import org.springframework.data.jpa.repository.Lock;

import javax.persistence.LockModeType;
import java.util.List;

public interface TOABAdvancedEntityBaseRepository<T> extends TOABSimpleEntityBaseRepository<T> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<T> findByNameContaining(String name);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByName(String name);

}
