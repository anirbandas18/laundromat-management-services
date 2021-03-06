package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.data.dto.TypeModelDto;
import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVException;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVVo;
import com.teenthofabud.laundromat.manager.tax.lov.repository.TaxLOVRepository;
import com.teenthofabud.laundromat.manager.tax.lov.service.TaxLOVService;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelDto;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelMessageTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class TaxModelDto2EntityConverter implements ComparativePatchConverter<TaxModelDto, TaxModelEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 7;

    private CurrencyTypeModelValidator currencyTypeModelValidator;

    private TaxLOVRepository taxLovRepository;
    private TaxLOVService taxLovService;

    @Autowired
    public void setTaxLovRepository(TaxLOVRepository taxLovRepository) {
        this.taxLovRepository = taxLovRepository;
    }

    @Autowired
    public void setTaxLovService(TaxLOVService taxLovService) {
        this.taxLovService = taxLovService;
    }

    @Autowired
    public void setCurrencyTypeModelValidator(CurrencyTypeModelValidator currencyTypeModelValidator) {
        this.currencyTypeModelValidator = currencyTypeModelValidator;
    }

    @Override
    public void compareAndMap(TaxModelDto dto, TaxModelEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optDescription = dto.getDescription();
        if(optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.description is valid");
        }
        Optional<String> optName = dto.getName();
        if(optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.name is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.active is valid");
        }
        Optional<String> optRate = dto.getRate();
        if(optRate.isPresent()) {
            actualEntity.setRate(Float.parseFloat(optRate.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.rate is valid");
        }
        Optional<String> optTaxLovId = dto.getTaxLovId();
        if(optTaxLovId.isPresent()) {
            Long taxLovId = Long.parseLong(optTaxLovId.get());
            try {
                TaxLOVVo taxLovVo = taxLovService.retrieveDetailsById(taxLovId);
                if(!taxLovVo.getActive()) {
                    log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_LOV_ID_INACTIVE.getValue());
                    throw new TaxModelException(TaxErrorCode.TAX_INACTIVE, new Object[] { "taxLovId" });
                }
            } catch (TaxLOVException e) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_LOV_ID_INVALID.getValue());
                log.error(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_LOV_ID_INVALID.getValue(), e);
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxLovId" });
            }
            Optional<TaxLOVEntity> optTaxLovEntity = taxLovRepository.findById(taxLovId);
            actualEntity.setTaxLov(optTaxLovEntity.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.taxLovId is valid");
        }
        Optional<TypeModelDto> optCurrencyTypeModel = dto.getCurrencyTypeModel();
        if(optCurrencyTypeModel.isPresent()) {
            TypeModelDto currencyTypeModelDto = optCurrencyTypeModel.get();
            TypeModelEntity currencyTypeModelEntity = new TypeModelEntity();
            Optional<String> optCurrencyTypeModelId = currencyTypeModelDto.getId();
            boolean isPresent = false;
            if(optCurrencyTypeModelId.isPresent()) {
                Long currencyTypeModelIdActual = Long.parseLong(optCurrencyTypeModelId.get());
                Errors internalErrors = new DirectFieldBindingResult(currencyTypeModelIdActual, "TaxModelDto.currencyTypeModel.id");
                currencyTypeModelValidator.validate(currencyTypeModelIdActual, internalErrors);
                if(internalErrors.hasErrors()) {
                    log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INVALID.getValue());
                    throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModel.id" });
                }
                currencyTypeModelEntity.setId(Long.parseLong(optCurrencyTypeModelId.get()));
                changeSW[i++] = isPresent = true;
                log.debug("TaxModelDto.currencyTypeModel.id is valid");
            }
            Optional<String> optCurrencyTypeModelName = currencyTypeModelDto.getName();
            if(optCurrencyTypeModelName.isPresent() && !StringUtils.isEmpty(optCurrencyTypeModelName.get())) {
                currencyTypeModelEntity.setName(optCurrencyTypeModelName.get());
                changeSW[i++] = isPresent = true;
                log.debug("TaxModelDto.currencyTypeModel.name is valid");
            }
            if(isPresent) {
                actualEntity.setCurrencyTypeModel(currencyTypeModelEntity);
            }
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TaxModelDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TaxModelDto attributes are valid");
    }
}
