package com.teenthofabud.laundromat.manager.type.model.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityConversionHandler;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TypeModelForm2EntityConverter extends TOABBaseEntityConversionHandler implements Converter<TypeModelForm, TypeModelEntity> {

    @Autowired
    public void setTypeLOVRepository(TypeLOVRepository typeLOVRepository) {
        this.typeLOVRepository = typeLOVRepository;
    }

    private TypeLOVRepository typeLOVRepository;

    @Override
    public TypeModelEntity convert(TypeModelForm form) {
        TypeModelEntity entity = new TypeModelEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        entity.setTypeLov(typeLOVRepository.findById(form.getTypeLovId()).get());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
