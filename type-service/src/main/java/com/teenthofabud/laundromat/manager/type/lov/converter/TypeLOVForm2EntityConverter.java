package com.teenthofabud.laundromat.manager.type.lov.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityConversionHandler;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TypeLOVForm2EntityConverter extends TOABBaseEntityConversionHandler implements Converter<TypeLOVForm, TypeLOVEntity> {

    @Override
    public TypeLOVEntity convert(TypeLOVForm form) {
        TypeLOVEntity entity = new TypeLOVEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
