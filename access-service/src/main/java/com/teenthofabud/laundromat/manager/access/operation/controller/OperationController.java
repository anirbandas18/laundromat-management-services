package com.teenthofabud.laundromat.manager.access.operation.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationForm;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationMessageTemplate;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
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
@RequestMapping("operation")
@Slf4j
@Tag(name = "Operation API", description = "Manage Operations and their details")
public class OperationController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(OperationService service) {
        this.service = service;
    }

    private OperationService service;

    @Operation(summary = "Create new Operation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "Operation attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Operation already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Operation attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewOperation(@RequestBody(required = false) OperationForm form) throws OperationException {
        log.debug("Requesting to create new operation");
        if(form != null) {
            Long id = service.createOperation(form);
            log.debug("Responding with identifier of newly created new operation");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("OperationForm is null");
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Operation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Operation attribute's value is invalid/Operation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Operation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Operation already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Operation details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingOperation(@PathVariable String id, @RequestBody(required = false) OperationForm form) throws OperationException {
        log.debug("Requesting to update all attributes of existing operation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateOperation(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing operation");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("OperationForm is null");
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_INVALID.getValue(), id);
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_EMPTY.getValue());
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Operation by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Operation id is invalid/Operation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Operation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Operation attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingOperation(@PathVariable String id) throws OperationException {
        log.debug("Requesting to soft delete operation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_VALID.getValue(), id);
                service.deleteOperation(actualId);
                log.debug("Responding with successful deletion of existing operation");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_INVALID.getValue(), id);
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_EMPTY.getValue());
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Operation attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Operation with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Operation attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Operation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Operation attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Operation with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingOperation(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws OperationException {
        log.debug("Requesting to patch of operation attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnOperation(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing operation");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("operation patch document is null");
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_INVALID.getValue(), id);
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_EMPTY.getValue());
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Operation details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Operations and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = OperationVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<OperationVo> getAllOperationNaturallyOrdered() {
        log.debug("Requesting all available operations by their natural orders");
        Set<OperationVo> naturallyOrderedOperations = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available operations by their natural orders");
        return naturallyOrderedOperations;
    }

    @Operation(summary = "Get all Operation details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Operations and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = OperationVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Operation name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Operations available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<OperationVo> getAllOperationsByName(@PathVariable String name) throws OperationException {
        log.debug("Requesting all available operations with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<OperationVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available operations with given name");
            return matchedByNames;
        }
        log.debug("operation name is empty");
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Operation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Operation that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = OperationVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Operation id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Operation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public OperationVo getOperationDetailsById(@PathVariable String id) throws OperationException {
        log.debug("Requesting all available operations by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_VALID.getValue(), id);
                OperationVo operationDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing operation details by id");
                return operationDetails;
            } catch (NumberFormatException e) {
                log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_INVALID.getValue(), id);
                throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_ID_EMPTY.getValue());
        throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
