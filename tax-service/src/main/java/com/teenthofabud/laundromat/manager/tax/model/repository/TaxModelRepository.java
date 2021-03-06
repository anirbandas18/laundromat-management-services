package com.teenthofabud.laundromat.manager.tax.model.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;


@Repository
public interface TaxModelRepository extends TOABAdvancedEntityBaseRepository<TaxModelEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TaxModelEntity save(TaxModelEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TaxModelEntity> findByTaxLovId(Long taxLovId);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TaxModelEntity> findByCurrencyTypeModelId(Long currencyTypeModelId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByNameAndTaxLovId(String name, Long taxLovId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByNameAndCurrencyTypeModelId(String name, Long currencyTypeModelId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByNameAndTaxLovIdAndCurrencyTypeModelId(String name, Long taxLovId, Long currencyTypeModelId);

}
