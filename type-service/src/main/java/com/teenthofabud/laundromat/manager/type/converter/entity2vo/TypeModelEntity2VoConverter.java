package com.teenthofabud.laundromat.manager.type.converter.entity2vo;

import com.teenthofabud.laundromat.manager.type.model.entity.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.vo.TypeModelVo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
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
        vo.setTypeLovVo(modelEntity2VoConverter.convert(entity.getTypeLov()));
        return vo;
    }
}
