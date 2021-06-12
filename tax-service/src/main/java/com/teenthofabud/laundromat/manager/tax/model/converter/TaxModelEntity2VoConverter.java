package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.proxy.TypeServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class TaxModelEntity2VoConverter implements Converter<TaxModelEntity, TaxModelVo> {

    @Autowired
    public void setTypeServiceClient(TypeServiceClient typeServiceClient) {
        this.typeServiceClient = typeServiceClient;
    }

    private TypeServiceClient typeServiceClient;

    @Override
    public TaxModelVo convert(TaxModelEntity entity) {
        TaxModelVo vo = new TaxModelVo();
        try {
            vo.setName(entity.getName());
            vo.setDescription(entity.getDescription());
            vo.setId(entity.getId());
            vo.setActive(entity.getActive());
            vo.setRate(entity.getRate());
            TypeModelVo internalTaxTypeModelVo = new TypeModelVo();
            com.teenthofabud.laundromat.manager.type.data.TypeModelVo taxTypeModelVo = typeServiceClient.getTypeModelDetailsById(entity.getTaxTypeModelId());
            log.debug("Retrieved: {}", taxTypeModelVo);
            internalTaxTypeModelVo.setId(taxTypeModelVo.getId());
            internalTaxTypeModelVo.setName(taxTypeModelVo.getName());
            vo.setTaxTypeModel(internalTaxTypeModelVo);
            TypeModelVo internalCurrencyTypeModelVo = new TypeModelVo();
            internalCurrencyTypeModelVo.setId(entity.getCurrencyTypeModel().getId());
            internalCurrencyTypeModelVo.setName(entity.getCurrencyTypeModel().getName());
            vo.setCurrencyTypeModel(internalCurrencyTypeModelVo);
            log.debug("Converted {} to {} ", entity, vo);
            return vo;
        } catch (TypeException e) {
            log.debug("Unable to convert {} to {}", entity, vo.getClass());
            log.error("Unable to convert TaxModel from Entity to Vo", e);
            throw new TOABSystemException("Unable to convert TaxModel from Entity to Vo", e);
        }
    }
}
