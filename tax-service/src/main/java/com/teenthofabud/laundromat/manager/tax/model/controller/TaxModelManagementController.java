package com.teenthofabud.laundromat.manager.tax.model.controller;

import com.teenthofabud.core.common.data.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.constant.TaxSubDomain;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import com.teenthofabud.laundromat.manager.tax.model.service.TaxModelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("taxmodel")
public class TaxModelManagementController {

    @Autowired
    public void setService(TaxModelService service) {
        this.service = service;
    }

    private TaxModelService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTaxModel(@RequestBody(required = false) TaxModelForm form) throws TaxException {
        if(form != null) {
            long id = service.createTaxModel(form);
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTaxModel(@PathVariable String id,
                                                           @RequestBody(required = false) TaxModelForm form) throws TaxException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(form != null) {
                    service.updateTaxModel(actualId, form);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTaxModel(@PathVariable String id) throws TaxException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                service.deleteTaxModel(actualId);
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTaxModel(@PathVariable String id,
                                                             @RequestBody(required = false) List<PatchOperationForm> dtoList) throws TOABBaseException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(dtoList != null) {
                    service.applyPatchOnTaxModel(actualId, dtoList);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TaxModelVo> getAllTaxModelNaturallyOrdered() {
        Set<TaxModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TaxModelVo> getAllStudentsByName(@PathVariable String name) throws TaxException {
        if(StringUtils.hasText(name)) {
            List<TaxModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            return matchedByNames;
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("id/{id}")
    public TaxModelVo getTaxModelDetailsById(@PathVariable String id) throws TaxException {
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                TaxModelVo studentDetails = service.retrieveDetailsById(actualId);
                return studentDetails;
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping("taxtypemodelid/{taxTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByTaxTypeModelId(@PathVariable String taxTypeModelId) throws TaxException {
        if(StringUtils.hasText(taxTypeModelId)) {
            try {
                Long actualTaxTypeModelId = Long.parseLong(taxTypeModelId);
                List<TaxModelVo> typeModelDetails = service.retrieveDetailsByTaxTypeModelId(actualTaxTypeModelId);
                return typeModelDetails;
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
    }

    @GetMapping("currencytypemodelid/{currencyTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByCurrencyTypeModelId(@PathVariable String currencyTypeModelId) throws TaxException {
        if(StringUtils.hasText(currencyTypeModelId)) {
            try {
                long actualCurrencyTypeModelId = Long.parseLong(currencyTypeModelId);
                List<TaxModelVo> typeModelDetails = service.retrieveDetailsByCurrencyTypeModelId(actualCurrencyTypeModelId);
                return typeModelDetails;
            } catch (NumberFormatException e) {
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
            }
        }
        throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
    }

}
