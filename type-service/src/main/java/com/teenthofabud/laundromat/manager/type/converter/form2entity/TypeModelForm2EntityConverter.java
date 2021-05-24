package com.teenthofabud.laundromat.manager.type.converter.form2entity;

import com.teenthofabud.core.common.handler.TOABBaseEntityConversionHandler;
import com.teenthofabud.laundromat.manager.type.model.entity.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.form.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.repository.TypeLOVRepository;
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
