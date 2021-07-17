package com.teenthofabud.laundromat.manager.access.operation.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class OperationForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<OperationForm, OperationEntity> {

    @Override
    public OperationEntity convert(OperationForm form) {
        OperationEntity entity = new OperationEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
