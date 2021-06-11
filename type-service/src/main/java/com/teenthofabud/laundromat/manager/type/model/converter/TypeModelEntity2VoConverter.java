package com.teenthofabud.laundromat.manager.type.model.converter;

import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.lov.converter.TypeLOVEntity2VoConverter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TypeModelEntity2VoConverter implements Converter<TypeModelEntity, TypeModelVo> {

    @Autowired
    public void setModelEntity2VoConverter(TypeLOVEntity2VoConverter modelEntity2VoConverter) {
        this.modelEntity2VoConverter = modelEntity2VoConverter;
    }

    private TypeLOVEntity2VoConverter modelEntity2VoConverter;

    @Override
    public TypeModelVo convert(TypeModelEntity entity) {
        TypeModelVo vo = new TypeModelVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        vo.setTypeLov(modelEntity2VoConverter.convert(entity.getTypeLov()));
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
