package com.teenthofabud.laundromat.manager.tax.lov.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TaxLOVForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<TaxLOVForm, TaxLOVEntity> {

    @Override
    public TaxLOVEntity convert(TaxLOVForm form) {
        TaxLOVEntity entity = new TaxLOVEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
