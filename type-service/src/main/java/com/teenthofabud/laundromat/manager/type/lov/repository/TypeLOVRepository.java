package com.teenthofabud.laundromat.manager.type.lov.repository;

import com.teenthofabud.core.common.repository.TOABJpaRepository;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface TypeLOVRepository extends TOABJpaRepository<TypeLOVEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TypeLOVEntity save(TypeLOVEntity entity);

}
