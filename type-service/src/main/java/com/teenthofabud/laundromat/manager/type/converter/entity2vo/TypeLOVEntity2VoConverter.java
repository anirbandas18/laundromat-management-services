package com.teenthofabud.laundromat.manager.type.converter.entity2vo;

import com.teenthofabud.laundromat.manager.type.model.entity.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.model.vo.TypeLOVVo;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class TypeLOVEntity2VoConverter implements Converter<TypeLOVEntity, TypeLOVVo> {
    @Override
    public TypeLOVVo convert(TypeLOVEntity entity) {
        TypeLOVVo vo = new TypeLOVVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        return vo;
    }
}
