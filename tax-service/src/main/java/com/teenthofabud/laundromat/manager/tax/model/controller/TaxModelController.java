package com.teenthofabud.laundromat.manager.tax.model.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelMessageTemplate;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import com.teenthofabud.laundromat.manager.tax.model.service.TaxModelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("model")
@Slf4j
@Tag(name = "Tax Model API", description = "Manage Tax Models and their details")
public class TaxModelController {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(TaxModelService service) {
        this.service = service;
    }

    private TaxModelService service;

    @Operation(summary = "Create new Tax Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Tax Model",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax Model attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Tax Model already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax Model attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Tax Model",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTaxModel(@RequestBody(required = false) TaxModelForm form) throws TaxModelException {
        log.debug("Requesting to create new tax model");
        if(form != null) {
            Long id = service.createTaxModel(form);
            log.debug("Responding with identifier of newly created new tax model");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TaxModelForm is null");
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Tax Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Tax Model",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax Model attribute's value is invalid/Type Model is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Tax Model already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Tax Model details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTaxModel(@PathVariable String id, @RequestBody(required = false) TaxModelForm form) throws TaxModelException {
        log.debug("Requesting to update all attributes of existing tax model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                if(form != null) {
                    service.updateTaxModel(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing tax model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TaxModelForm is null");
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Tax Model by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Tax Model",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax Model id is invalid/Type Model is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax Model attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Tax Model",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTaxModel(@PathVariable String id) throws TaxModelException {
        log.debug("Requesting to soft delete tax model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                service.deleteTaxModel(actualId);
                log.debug("Responding with successful deletion of existing tax model");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Tax Model attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Tax Model with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax Model attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax Model attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Tax Model with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_TYPE_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingTaxModel(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TOABBaseException {
        log.debug("Requesting to patch of tax model attributes");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                if(dtoList != null) {
                    service.applyPatchOnTaxModel(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing tax model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("tax model patch document is null");
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Tax Model details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tax Models and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TaxModelVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<TaxModelVo> getAllTaxModelNaturallyOrdered() {
        log.debug("Requesting all available tax models by their natural orders");
        Set<TaxModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available tax models by their natural orders");
        return naturallyOrderedStudents;
    }

    @Operation(summary = "Get all Tax Model details by name")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tax Models and their details that match the given name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(schema = @Schema(implementation = TaxModelVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Tax Model name is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Models available with the given name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<TaxModelVo> getAllStudentsByName(@PathVariable String name) throws TaxModelException {
        log.debug("Requesting all available tax models with given name");
        if(StringUtils.hasText(name)) {
            List<TaxModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available tax models with given name");
            return matchedByNames;
        }
        log.debug("tax model name is empty");
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Tax Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Tax Model that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TaxModelVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax Model id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TaxModelVo getTaxModelDetailsById(@PathVariable String id) throws TaxModelException {
        log.debug("Requesting all available tax models by its id");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_VALID, id);
                TaxModelVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing tax model details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_INVALID, id);
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_ID_EMPTY);
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Tax Model details by taxTypeModelId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Tax Model that belong to the given taxTypeModelId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(schema = @Schema(implementation = TaxModelVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Tax Model taxTypeModelId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model belongs to the given taxTypeModelId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("taxtypemodelid/{taxTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByTaxTypeModelId(@PathVariable String taxTypeModelId) throws TaxModelException {
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
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
            }
        }
        log.debug("taxTypeModelId is empty");
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "taxTypeModelId", taxTypeModelId });
    }

    @Operation(summary = "Get all Tax Model details by currencyTypeModelId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Tax Model that belong to the given currencyTypeModelId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(schema = @Schema(implementation = TaxModelVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Tax Model currencyTypeModelId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax Model belongs to the given currencyTypeModelId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("currencytypemodelid/{currencyTypeModelId}")
    public List<TaxModelVo> getTaxModelDetailsByCurrencyTypeModelId(@PathVariable String currencyTypeModelId) throws TaxModelException {
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
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
            }
        }
        log.debug("currencyTypeModelId is empty");
        throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "currencyTypeModelId", currencyTypeModelId });
    }

}
