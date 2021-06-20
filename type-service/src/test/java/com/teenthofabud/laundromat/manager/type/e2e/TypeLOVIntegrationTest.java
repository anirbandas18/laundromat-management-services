package com.teenthofabud.laundromat.manager.type.e2e;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class TypeLOVIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String TYPE_LOV_URI = "/lov";
    private static final String TYPE_LOV_URI_BY_ID = "/lov/{id}";
    private static final String TYPE_LOV_URI_BY_NAME = "/lov/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private TypeLOVForm typeLOVForm;
    private TypeLOVVo typeLOVVo1;
    private TypeLOVVo typeLOVVo2;
    private TypeLOVVo typeLOVVo3;
    private TypeLOVVo typeLOVVo4;
    private TypeLOVEntity typeLOVEntity1;
    private TypeLOVEntity typeLOVEntity2;
    private TypeLOVEntity typeLOVEntity3;
    private TypeLOVEntity typeLOVEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        typeLOVForm = new TypeLOVForm();
        typeLOVForm.setName("Demo LOV");
        typeLOVForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample LOV"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Type LOV resource"));

        typeLOVVo1 = new TypeLOVVo();
        typeLOVVo1.setId(1L);
        typeLOVVo1.setActive(Boolean.TRUE);
        typeLOVVo1.setName("Test Type LOV 1");
        typeLOVVo1.setDescription("This belongs to group 1 for e2e testing");

        typeLOVVo2 = new TypeLOVVo();
        typeLOVVo2.setId(2L);
        typeLOVVo2.setActive(Boolean.TRUE);
        typeLOVVo2.setName("Test Type LOV 2");
        typeLOVVo2.setDescription("This belongs to group 1 for e2e testing");

        typeLOVVo3 = new TypeLOVVo();
        typeLOVVo3.setId(3L);
        typeLOVVo3.setActive(Boolean.TRUE);
        typeLOVVo3.setName("Sample Type LOV 3");
        typeLOVVo3.setDescription("This belongs to group 2 for e2e testing");

        typeLOVVo4 = new TypeLOVVo();
        typeLOVVo4.setId(4L);
        typeLOVVo4.setActive(Boolean.FALSE);
        typeLOVVo4.setName("Sample Type LOV 4");
        typeLOVVo4.setDescription("This belongs to group 2 for e2e testing");

        typeLOVEntity1 = new TypeLOVEntity();
        typeLOVEntity1.setActive(Boolean.TRUE);
        typeLOVEntity1.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity1.setCreatedOn(LocalDateTime.now());
        typeLOVEntity1.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity1.setModifiedOn(LocalDateTime.now());
        typeLOVEntity1.setVersion(0);
        typeLOVEntity1.setName("Test Type LOV 1");
        typeLOVEntity1.setDescription("This belongs to group 1 for e2e testing");

        typeLOVEntity2 = new TypeLOVEntity();
        typeLOVEntity2.setActive(Boolean.TRUE);
        typeLOVEntity2.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity2.setCreatedOn(LocalDateTime.now());
        typeLOVEntity2.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity2.setModifiedOn(LocalDateTime.now());
        typeLOVEntity2.setVersion(0);
        typeLOVEntity2.setName("Test Type LOV 2");
        typeLOVEntity2.setDescription("This belongs to group 1 for e2e testing");


        typeLOVEntity3 = new TypeLOVEntity();
        typeLOVEntity3.setActive(Boolean.TRUE);
        typeLOVEntity3.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity3.setCreatedOn(LocalDateTime.now());
        typeLOVEntity3.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity3.setModifiedOn(LocalDateTime.now());
        typeLOVEntity3.setVersion(0);
        typeLOVEntity3.setName("Sample Type LOV 3");
        typeLOVEntity3.setDescription("This belongs to group 2 for e2e testing");


        typeLOVEntity4 = new TypeLOVEntity();
        typeLOVEntity4.setActive(Boolean.FALSE);
        typeLOVEntity4.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity4.setCreatedOn(LocalDateTime.now());
        typeLOVEntity4.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity4.setModifiedOn(LocalDateTime.now());
        typeLOVEntity4.setVersion(0);
        typeLOVEntity4.setName("Sample Type LOV 4");
        typeLOVEntity4.setDescription("This belongs to group 2 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(typeLOVEntity1);
        em.merge(typeLOVEntity2);
        em.merge(typeLOVEntity3);
        em.merge(typeLOVEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(typeLOVEntity1);
        em.remove(typeLOVEntity2);
        em.remove(typeLOVEntity3);
        em.remove(typeLOVEntity4);

        typeLOVForm = new TypeLOVForm();
        typeLOVForm.setName("Demo LOV");
        typeLOVForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample LOV"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Type LOV resource"));
    }

    @Test
    public void test_TypeLOV_Post_ShouldReturn_200Response_And_NewTypeLOVId_WhenPosted_WithValidTypeLOVForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TYPE_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TypeLOV_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        typeLOVForm.setName("");

        mvcResult = mockMvc.perform(post(TYPE_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeLOV_Post_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_004_WhenRequested_WithDuplicateTypeLOV() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String field1Name = "name";
        typeLOVForm.setName("Test Type LOV 1");

        mvcResult = mockMvc.perform(post(TYPE_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TypeLOV_Post_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenPosted_WithNoTypeLOVForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TYPE_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_200Response_And_TypeLOVListNaturallyOrdered_WhenRequested_ForAllTypeLOVs() throws Exception {
        MvcResult mvcResult = null;
        Set<TypeLOVVo> studentList = new TreeSet<>(Arrays.asList(typeLOVVo1, typeLOVVo2, typeLOVVo3, typeLOVVo4));
        long expectedTypeLOVVoCount = 4;

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedTypeLOVVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), TypeLOVVo[].class).length);
    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_200Response_And_TypeLOVDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(typeLOVVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(typeLOVVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TypeLOVVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TypeLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_200Response_And_MatchingTypeLOVDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<TypeLOVVo> students = Arrays.asList(typeLOVVo1, typeLOVVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), TypeLOVVo[].class).length);
    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Get_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TYPE_LOV_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_TypeLOV_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TypeLOV_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_LOV_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_LOV_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeLOV_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        typeLOVForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TypeLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdatedBy_EmptyInvalidId_AndTypeLOVDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ByAbsentId_AndTypeLOVDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_005_WhenUpdated_ByInactiveId_AndTypeLOVDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndNoTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        typeLOVForm.setName("");

        mvcResult = mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndEmptyTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TypeLOVForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeLOV_Put_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_004_WhenUpdated_ById_AndDuplicateTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String field1Name = "name";
        typeLOVForm.setName(typeLOVEntity1.getName());

        mvcResult = mockMvc.perform(put(TYPE_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeLOVForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTypeLOVDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTypeLOVDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ByInvalidId_AndTypeLOVDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ByAbsentId_AndTypeLOVDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ById_AndDuplicateTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample Type LOV 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndNoTypeLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidDefinitionOfTypeLOVAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TYPE_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}