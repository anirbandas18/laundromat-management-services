package com.teenthofabud.core.common.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import javax.persistence.LockModeType;
import java.util.List;
import java.util.Optional;

public interface TOABSimpleEntityBaseRepository<T> extends JpaRepository<T, Long> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<T> findAll();

    @Lock(LockModeType.PESSIMISTIC_READ)
    public Optional<T> findById(Long id);

}
