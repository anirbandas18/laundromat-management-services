package com.teenthofabud.laundromat.manager.type.repository;

import com.teenthofabud.core.common.repository.TOABJpaRepository;
import com.teenthofabud.laundromat.manager.type.model.entity.TypeModelEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;


@Repository
public interface TypeModelRepository extends TOABJpaRepository<TypeModelEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TypeModelEntity save(TypeModelEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TypeModelEntity> findByTypeLovId(Long typeLovId);

}
