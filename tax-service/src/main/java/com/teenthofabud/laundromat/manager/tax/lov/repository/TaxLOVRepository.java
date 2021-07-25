package com.teenthofabud.laundromat.manager.tax.lov.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface TaxLOVRepository extends TOABAdvancedEntityBaseRepository<TaxLOVEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TaxLOVEntity save(TaxLOVEntity entity);

}
