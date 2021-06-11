package com.teenthofabud.laundromat.manager.type.model.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;


@Repository
public interface TypeModelRepository extends TOABAdvancedEntityBaseRepository<TypeModelEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TypeModelEntity save(TypeModelEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TypeModelEntity> findByTypeLovId(Long typeLovId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByNameAndTypeLovId(String name, Long typeLovId);

}
