package com.teenthofabud.laundromat.manager.tax.lov.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVException;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVMessageTemplate;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVVo;
import com.teenthofabud.laundromat.manager.tax.lov.service.TaxLOVService;
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
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("lov")
@Slf4j
@Tag(name = "Tax LOV API", description = "Manage Tax LOVs and their details")
public class TaxLOVController {

    private static final String MEDIA_TAX_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(TaxLOVService service) {
        this.service = service;
    }

    private TaxLOVService service;

    @Operation(summary = "Create new Tax LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Tax LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Long.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax LOV attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Tax LOV already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax LOV attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Tax LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTaxLOV(@RequestBody(required = false) TaxLOVForm form) throws TaxLOVException {
        log.debug("Requesting to create new tax LOV");
        if(form != null) {
            Long id = service.createTaxLOV(form);
            log.debug("Responding with identifier of newly created new tax LOV");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TaxLOVForm is null");
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Tax LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Tax LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax LOV attribute's value is invalid/Tax LOV is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Tax LOV already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Tax LOV details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTaxLOV(@PathVariable String id, @RequestBody(required = false) TaxLOVForm form) throws TaxLOVException {
        log.debug("Requesting to update all attributes of existing tax LOV");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateTaxLOV(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing tax LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TaxLOVForm is null");
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_INVALID.getValue(), id);
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_EMPTY.getValue());
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Tax LOV by id and all associated Tax Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Tax LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax LOV id is invalid/Tax LOV is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax LOV attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Tax LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTaxLOV(@PathVariable String id) throws TaxLOVException {
        log.debug("Requesting to soft delete tax LOV");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_VALID.getValue(), id);
                service.deleteTaxLOV(actualId);
                log.debug("Responding with successful deletion of existing tax LOV");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_INVALID.getValue(), id);
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_EMPTY.getValue());
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Tax LOV attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Tax LOV with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax LOV attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Tax LOV attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Tax LOV with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_TAX_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingTaxLOV(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TaxLOVException {
        log.debug("Requesting to patch of tax LOV attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnTaxLOV(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing tax LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("tax LOV patch document is null");
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_INVALID.getValue(), id);
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_EMPTY.getValue());
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Tax LOV details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tax LOVs and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TaxLOVVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<TaxLOVVo> getAllTaxLOVNaturallyOrdered() {
        log.debug("Requesting all available tax LOVs by their natural orders");
        Set<TaxLOVVo> naturallyOrderedTaxLOVs = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available tax LOVs by their natural orders");
        return naturallyOrderedTaxLOVs;
    }

    @Operation(summary = "Get all Tax LOV details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Tax LOVs and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TaxLOVVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Tax LOV name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Tax LOVs available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<TaxLOVVo> getAllTaxLOVsByName(@PathVariable String name) throws TaxLOVException {
        log.debug("Requesting all available tax LOVs with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<TaxLOVVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available tax LOVs with given name");
            return matchedByNames;
        }
        log.debug("tax LOV name is empty");
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Tax LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Tax LOV that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TaxLOVVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Tax LOV id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tax LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TaxLOVVo getTaxLOVDetailsById(@PathVariable String id) throws TaxLOVException {
        log.debug("Requesting all available tax LOVs by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_VALID.getValue(), id);
                TaxLOVVo taxLovDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing tax LOV details by id");
                return taxLovDetails;
            } catch (NumberFormatException e) {
                log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_INVALID.getValue(), id);
                throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_ID_EMPTY.getValue());
        throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
