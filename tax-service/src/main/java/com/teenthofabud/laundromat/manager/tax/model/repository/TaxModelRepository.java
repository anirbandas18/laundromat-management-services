package com.teenthofabud.laundromat.manager.tax.model.repository;

import com.teenthofabud.core.common.repository.TOABJpaRepository;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;


@Repository
public interface TaxModelRepository extends TOABJpaRepository<TaxModelEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TaxModelEntity save(TaxModelEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TaxModelEntity> findByTaxTypeModelId(Long taxTypeModelId);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TaxModelEntity> findByCurrencyTypeModelId(Long currencyTypeModelId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByTaxTypeModelId(Long taxTypeModelId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByNameAndTaxTypeModelId(String name, Long taxTypeModelId);

}
