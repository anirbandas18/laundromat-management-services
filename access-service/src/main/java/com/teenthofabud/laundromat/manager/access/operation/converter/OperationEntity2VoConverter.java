package com.teenthofabud.laundromat.manager.access.operation.converter;

import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class OperationEntity2VoConverter implements Converter<OperationEntity, OperationVo> {
    @Override
    public OperationVo convert(OperationEntity entity) {
        OperationVo vo = new OperationVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
