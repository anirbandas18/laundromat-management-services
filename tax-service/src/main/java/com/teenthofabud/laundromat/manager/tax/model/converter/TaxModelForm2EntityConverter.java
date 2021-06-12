package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TaxModelForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<TaxModelForm, TaxModelEntity> {

    @Override
    public TaxModelEntity convert(TaxModelForm form) {
        TaxModelEntity entity = new TaxModelEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        entity.setRate(form.getRate());
        entity.setTaxTypeModelId(form.getTaxTypeModelId());
        TypeModelEntity currencyTypeModel = new TypeModelEntity();
        currencyTypeModel.setId(form.getCurrencyTypeModel().getId());
        currencyTypeModel.setName(form.getCurrencyTypeModel().getName());
        entity.setCurrencyTypeModel(currencyTypeModel);
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
