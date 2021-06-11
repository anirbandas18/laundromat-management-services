package com.teenthofabud.laundromat.manager.type.lov.converter;

import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TypeLOVEntity2VoConverter implements Converter<TypeLOVEntity, TypeLOVVo> {
    @Override
    public TypeLOVVo convert(TypeLOVEntity entity) {
        TypeLOVVo vo = new TypeLOVVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
