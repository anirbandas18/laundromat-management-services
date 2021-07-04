package com.teenthofabud.laundromat.manager.type.lov.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVMessageTemplate;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
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
@RequestMapping("lov")
@Slf4j
@Tag(name = "Type LOV API", description = "Manage Type LOVs and their details")
public class TypeLOVController {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(TypeLOVService service) {
        this.service = service;
    }

    private TypeLOVService service;

    @Operation(summary = "Create new Type LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type LOV attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Type LOV already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type LOV attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeLOV(@RequestBody(required = false) TypeLOVForm form) throws TypeLOVException {
        log.debug("Requesting to create new type LOV");
        if(form != null) {
            Long id = service.createTypeLOV(form);
            log.debug("Responding with identifier of newly created new type LOV");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TypeLOVForm is null");
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Type LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type LOV attribute's value is invalid/Type LOV is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Type LOV already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Type LOV details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeLOV(@PathVariable String id, @RequestBody(required = false) TypeLOVForm form) throws TypeLOVException {
        log.debug("Requesting to update all attributes of existing type LOV");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_VALID, id);
                if(form != null) {
                    service.updateTypeLOV(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing type LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TypeLOVForm is null");
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_INVALID, id);
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_EMPTY);
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Type LOV by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type LOV id is invalid/Type LOV is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type LOV attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeLOV(@PathVariable String id) throws TypeLOVException {
        log.debug("Requesting to soft delete type LOV");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_VALID, id);
                service.deleteTypeLOV(actualId);
                log.debug("Responding with successful deletion of existing type LOV");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_INVALID, id);
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_EMPTY);
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Type LOV attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Type LOV with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type LOV attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type LOV attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Type LOV with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_TYPE_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingTypeLOV(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TypeLOVException {
        log.debug("Requesting to patch of type LOV attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_VALID, id);
                if(dtoList != null) {
                    service.applyPatchOnTypeLOV(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing type LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("type LOV patch document is null");
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_INVALID, id);
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_EMPTY);
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Type LOV details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Type LOVs and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TypeLOVVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<TypeLOVVo> getAllTypeLOVNaturallyOrdered() {
        log.debug("Requesting all available type LOVs by their natural orders");
        Set<TypeLOVVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available type LOVs by their natural orders");
        return naturallyOrderedStudents;
    }

    @Operation(summary = "Get all Type LOV details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Type LOVs and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TypeLOVVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Type LOV name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Type LOVs available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<TypeLOVVo> getAllStudentsByName(@PathVariable String name) throws TypeLOVException {
        log.debug("Requesting all available type LOVs with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<TypeLOVVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available type LOVs with given name");
            return matchedByNames;
        }
        log.debug("type LOV name is empty");
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Type LOV details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Type LOV that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TypeLOVVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Type LOV id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type LOV found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TypeLOVVo getTypeLOVDetailsById(@PathVariable String id) throws TypeLOVException {
        log.debug("Requesting all available type LOVs by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_VALID, id);
                TypeLOVVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing type LOV details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_INVALID, id);
                throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_ID_EMPTY);
        throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
