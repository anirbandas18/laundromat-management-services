package com.teenthofabud.laundromat.manager.tax.lov.converter;

import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TaxLOVEntity2VoConverter implements Converter<TaxLOVEntity, TaxLOVVo> {
    @Override
    public TaxLOVVo convert(TaxLOVEntity entity) {
        TaxLOVVo vo = new TaxLOVVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
