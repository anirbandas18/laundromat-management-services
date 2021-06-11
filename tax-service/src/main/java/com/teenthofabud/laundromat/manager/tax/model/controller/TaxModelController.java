package com.teenthofabud.laundromat.manager.tax.model.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.constant.TaxMessageTemplate;
import com.teenthofabud.laundromat.manager.tax.constant.TaxSubDomain;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import com.teenthofabud.laundromat.manager.tax.model.service.TaxModelService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("model")
@Slf4j
public class TaxModelController {

    @Autowired
    public void setService(TaxModelService service) {
        this.service = service;
    }

    private TaxModelService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTaxModel(@RequestBody(required = false) TaxModelForm form) throws TaxException {
        log.debug("Requesting to create new tax model");
        if(form != null) {
            Long id = service.createTaxModel(form);
            log.debug("Responding with identifier of newly created new tax model");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TaxModelForm is null");
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTaxModel(@PathVariable String id, @RequestBody(required = false) TaxModelForm form) throws TaxException {
        log.debug("Requesting to update all attributes of existing tax model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                if(form != null) {
                    service.updateTaxModel(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing tax model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TaxModelForm is null");
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTaxModel(@PathVariable String id) throws TaxException {
        log.debug("Requesting to soft delete tax model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                service.deleteTaxModel(actualId);
                log.debug("Responding with successful deletion of existing tax model");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTaxModel(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TOABBaseException {
        log.debug("Requesting to patch of tax model attributes");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                if(dtoList != null) {
                    service.applyPatchOnTaxModel(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing tax model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("tax model patch document is null");
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TaxModelVo> getAllTaxModelNaturallyOrdered() {
        log.debug("Requesting all available tax models by their natural orders");
        Set<TaxModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available tax models by their natural orders");
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TaxModelVo> getAllStudentsByName(@PathVariable String name) throws TaxException {
        log.debug("Requesting all available tax models with given name");
        if(StringUtils.hasText(name)) {
            List<TaxModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available tax models with given name");
            return matchedByNames;
        }
        log.debug("tax model name is empty");
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("{id}")
    public TaxModelVo getTaxModelDetailsById(@PathVariable String id) throws TaxException {
        log.debug("Requesting all available tax models by its id");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                TaxModelVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing tax model details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping("taxtypemodelid/{taxTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByTaxTypeModelId(@PathVariable String taxTypeModelId) throws TaxException {
        log.debug("Requesting all available tax models by taxTypeModelId");
        if(StringUtils.hasText(taxTypeModelId)) {
            try {
                Long actualTaxTypeModelId = Long.parseLong(taxTypeModelId);
                log.debug("taxTypeModelId: {} is semantically valid", taxTypeModelId);
                List<TaxModelVo> taxModelDetails = service.retrieveDetailsByTaxTypeModelId(actualTaxTypeModelId);
                log.debug("Responding with successful retrieval of all available tax model details by taxTypeModelId");
                return taxModelDetails;
            } catch (NumberFormatException e) {
                log.debug("taxTypeModelId: {} is invalid", taxTypeModelId);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
            }
        }
        log.debug("taxTypeModelId is empty");
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
    }

    @GetMapping("currencytypemodelid/{currencyTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByCurrencyTypeModelId(@PathVariable String currencyTypeModelId) throws TaxException {
        log.debug("Requesting all available tax models by currencyTypeModelId");
        if(StringUtils.hasText(currencyTypeModelId)) {
            try {
                Long actualCurrencyTypeModelId = Long.parseLong(currencyTypeModelId);
                log.debug("currencyTypeModelId: {} is semantically valid", currencyTypeModelId);
                List<TaxModelVo> taxModelDetails = service.retrieveDetailsByCurrencyTypeModelId(actualCurrencyTypeModelId);
                log.debug("Responding with successful retrieval of all available tax model details by currencyTypeModelId");
                return taxModelDetails;
            } catch (NumberFormatException e) {
                log.debug("currencyTypeModelId: {} is invalid", currencyTypeModelId);
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
            }
        }
        log.debug("currencyTypeModelId is empty");
        throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
    }

}
