package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.repository.TaxLOVRepository;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class TaxModelForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<TaxModelForm, TaxModelEntity> {

    private TaxLOVRepository taxLovRepository;

    @Autowired
    public void setTaxLovRepository(TaxLOVRepository taxLovRepository) {
        this.taxLovRepository = taxLovRepository;
    }

    @Override
    public TaxModelEntity convert(TaxModelForm form) {
        TaxModelEntity entity = new TaxModelEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        entity.setRate(form.getRate());
        Optional<TaxLOVEntity> optTaxLovEntity = taxLovRepository.findById(form.getTaxLovId());
        entity.setTaxLov(optTaxLovEntity.get());
        TypeModelEntity currencyTypeModel = new TypeModelEntity();
        currencyTypeModel.setId(form.getCurrencyTypeModel().getId());
        currencyTypeModel.setName(form.getCurrencyTypeModel().getName());
        entity.setCurrencyTypeModel(currencyTypeModel);
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
