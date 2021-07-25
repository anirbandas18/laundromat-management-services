package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TaxModelEntity2VoConverter implements Converter<TaxModelEntity, TaxModelVo> {

    @Override
    public TaxModelVo convert(TaxModelEntity entity) {
        TaxModelVo vo = new TaxModelVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        vo.setRate(entity.getRate());
        TypeModelVo taxLovVo = new TypeModelVo();
        taxLovVo.setId(entity.getTaxLov().getId());
        taxLovVo.setName(entity.getTaxLov().getName());
        vo.setTaxLov(taxLovVo);
        TypeModelVo internalCurrencyTypeModelVo = new TypeModelVo();
        internalCurrencyTypeModelVo.setId(entity.getCurrencyTypeModel().getId());
        internalCurrencyTypeModelVo.setName(entity.getCurrencyTypeModel().getName());
        vo.setCurrencyTypeModel(internalCurrencyTypeModelVo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
