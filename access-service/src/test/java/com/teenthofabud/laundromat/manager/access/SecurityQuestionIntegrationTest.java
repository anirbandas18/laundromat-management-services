package com.teenthofabud.laundromat.manager.access;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionForm;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionVo;
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

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class SecurityQuestionIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String SECURITY_QUESTION_URI = "/securityquestion";
    private static final String SECURITY_QUESTION_URI_BY_ID = "/securityquestion/{id}";
    private static final String SECURITY_QUESTION_URI_BY_NAME = "/securityquestion/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private SecurityQuestionForm securityQuestionForm;
    private SecurityQuestionVo securityQuestionVo1;
    private SecurityQuestionVo securityQuestionVo2;
    private SecurityQuestionVo securityQuestionVo3;
    private SecurityQuestionVo securityQuestionVo4;
    private SecurityQuestionEntity securityQuestionEntity1;
    private SecurityQuestionEntity securityQuestionEntity2;
    private SecurityQuestionEntity securityQuestionEntity3;
    private SecurityQuestionEntity securityQuestionEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        securityQuestionForm = new SecurityQuestionForm();
        securityQuestionForm.setName("Demo SecurityQuestion");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample SecurityQuestion"),
                new PatchOperationForm("replace", "/active", "true"));

        securityQuestionVo1 = new SecurityQuestionVo();
        securityQuestionVo1.setId(1L);
        securityQuestionVo1.setActive(Boolean.TRUE);
        securityQuestionVo1.setName("Test SecurityQuestion 1");

        securityQuestionVo2 = new SecurityQuestionVo();
        securityQuestionVo2.setId(2L);
        securityQuestionVo2.setActive(Boolean.TRUE);
        securityQuestionVo2.setName("Test SecurityQuestion 2");

        securityQuestionVo3 = new SecurityQuestionVo();
        securityQuestionVo3.setId(3L);
        securityQuestionVo3.setActive(Boolean.TRUE);
        securityQuestionVo3.setName("Sample SecurityQuestion 3");

        securityQuestionVo4 = new SecurityQuestionVo();
        securityQuestionVo4.setId(4L);
        securityQuestionVo4.setActive(Boolean.FALSE);
        securityQuestionVo4.setName("Sample SecurityQuestion 4");

        securityQuestionEntity1 = new SecurityQuestionEntity();
        securityQuestionEntity1.setActive(Boolean.TRUE);
        securityQuestionEntity1.setCreatedBy(CREATED_BY_USER_ID);
        securityQuestionEntity1.setCreatedOn(LocalDateTime.now());
        securityQuestionEntity1.setModifiedBy(CREATED_BY_USER_ID);
        securityQuestionEntity1.setModifiedOn(LocalDateTime.now());
        securityQuestionEntity1.setVersion(0);
        securityQuestionEntity1.setName("Test SecurityQuestion 1");

        securityQuestionEntity2 = new SecurityQuestionEntity();
        securityQuestionEntity2.setActive(Boolean.TRUE);
        securityQuestionEntity2.setCreatedBy(CREATED_BY_USER_ID);
        securityQuestionEntity2.setCreatedOn(LocalDateTime.now());
        securityQuestionEntity2.setModifiedBy(CREATED_BY_USER_ID);
        securityQuestionEntity2.setModifiedOn(LocalDateTime.now());
        securityQuestionEntity2.setVersion(0);
        securityQuestionEntity2.setName("Test SecurityQuestion 2");

        securityQuestionEntity3 = new SecurityQuestionEntity();
        securityQuestionEntity3.setActive(Boolean.TRUE);
        securityQuestionEntity3.setCreatedBy(CREATED_BY_USER_ID);
        securityQuestionEntity3.setCreatedOn(LocalDateTime.now());
        securityQuestionEntity3.setModifiedBy(CREATED_BY_USER_ID);
        securityQuestionEntity3.setModifiedOn(LocalDateTime.now());
        securityQuestionEntity3.setVersion(0);
        securityQuestionEntity3.setName("Sample SecurityQuestion 3");

        securityQuestionEntity4 = new SecurityQuestionEntity();
        securityQuestionEntity4.setActive(Boolean.FALSE);
        securityQuestionEntity4.setCreatedBy(CREATED_BY_USER_ID);
        securityQuestionEntity4.setCreatedOn(LocalDateTime.now());
        securityQuestionEntity4.setModifiedBy(CREATED_BY_USER_ID);
        securityQuestionEntity4.setModifiedOn(LocalDateTime.now());
        securityQuestionEntity4.setVersion(0);
        securityQuestionEntity4.setName("Sample SecurityQuestion 4");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(securityQuestionEntity1);
        em.merge(securityQuestionEntity2);
        em.merge(securityQuestionEntity3);
        em.merge(securityQuestionEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(securityQuestionEntity1);
        em.remove(securityQuestionEntity2);
        em.remove(securityQuestionEntity3);
        em.remove(securityQuestionEntity4);

        securityQuestionForm = new SecurityQuestionForm();
        securityQuestionForm.setName("Demo SecurityQuestion");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample SecurityQuestion"),
                new PatchOperationForm("replace", "/active", "true"));
    }

    @Test
    public void test_SecurityQuestion_Post_ShouldReturn_201Response_And_NewSecurityQuestionId_WhenPosted_WithValidSecurityQuestionForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(SECURITY_QUESTION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_SecurityQuestion_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        securityQuestionForm.setName("");

        mvcResult = mockMvc.perform(post(SECURITY_QUESTION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_SecurityQuestion_Post_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenRequested_WithDuplicateSecurityQuestion() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        securityQuestionForm.setName("Test SecurityQuestion 1");

        mvcResult = mockMvc.perform(post(SECURITY_QUESTION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_SecurityQuestion_Post_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenPosted_WithNoSecurityQuestionForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(SECURITY_QUESTION_URI)
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
    public void test_SecurityQuestion_Get_ShouldReturn_200Response_And_SecurityQuestionListNaturallyOrdered_WhenRequested_ForAllSecurityQuestions() throws Exception {
        MvcResult mvcResult = null;
        Set<SecurityQuestionVo> studentList = new TreeSet<>(Arrays.asList(securityQuestionVo1, securityQuestionVo2, securityQuestionVo3, securityQuestionVo4));
        long expectedSecurityQuestionVoCount = 4;

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedSecurityQuestionVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), SecurityQuestionVo[].class).length);
    }

    @Test
    public void test_SecurityQuestion_Get_ShouldReturn_200Response_And_SecurityQuestionDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(securityQuestionVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(securityQuestionVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), SecurityQuestionVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_SecurityQuestion_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Get_ShouldReturn_200Response_And_MatchingSecurityQuestionDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<SecurityQuestionVo> students = Arrays.asList(securityQuestionVo1, securityQuestionVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), SecurityQuestionVo[].class).length);
    }

    @Test
    public void test_SecurityQuestion_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(SECURITY_QUESTION_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_SecurityQuestion_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_SecurityQuestion_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(SECURITY_QUESTION_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(SECURITY_QUESTION_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_SecurityQuestion_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndSecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        securityQuestionForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_SecurityQuestion_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenUpdatedBy_EmptyInvalidId_AndSecurityQuestionDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndSecurityQuestionDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenUpdated_ByInactiveId_AndSecurityQuestionDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoSecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        securityQuestionForm.setName("");

        mvcResult = mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndEmptySecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new SecurityQuestionForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_SecurityQuestion_Put_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenUpdated_ById_AndDuplicateSecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        securityQuestionForm.setName(securityQuestionEntity2.getName());

        mvcResult = mockMvc.perform(put(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(securityQuestionForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_SecurityQuestion_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndSecurityQuestionDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_SecurityQuestion_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndSecurityQuestionDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, " ")
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
    public void test_SecurityQuestion_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ByInvalidId_AndSecurityQuestionDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndSecurityQuestionDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ById_AndDuplicateSecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample SecurityQuestion 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoSecurityQuestionDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_SecurityQuestion_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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
    public void test_SecurityQuestion_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidDefinitionOfSecurityQuestionAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(SECURITY_QUESTION_URI_BY_ID, id)
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