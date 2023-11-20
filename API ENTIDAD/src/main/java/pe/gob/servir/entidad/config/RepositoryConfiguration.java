package pe.gob.servir.entidad.config;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
		entityManagerFactoryRef = "entidadEntityManagerFactory", 
		transactionManagerRef = "entidadTransactionManager", 
		basePackages = { "pe.gob.servir.entidad.repository" })
public class RepositoryConfiguration {

	@Primary
	@Bean(name = "entidadDataSourceProperties")
	@ConfigurationProperties("entidad.datasource")
	public DataSourceProperties dataSourceProperties() {
		return new DataSourceProperties();
	}

	@Primary
	@Bean(name = "entidadDataSource")
	@ConfigurationProperties("entidad.datasource.configuration")
	public DataSource dataSource(
			@Qualifier("entidadDataSourceProperties") DataSourceProperties entidadDataSourceProperties) {
		return entidadDataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
	}

	@Primary
	@Bean(name = "entidadEntityManagerFactory")
	public LocalContainerEntityManagerFactoryBean entityManagerFactory(EntityManagerFactoryBuilder builder,
			@Qualifier("entidadDataSource") DataSource entidadDataSource) {
		Map<String, Object> properties = new HashMap<>();
//		properties.put("hibernate.physical_naming_strategy", "pe.gob.servir.entidad.common.OracleSQLNamingStrategy");
		properties.put("hibernate.format_sql", "true");
		properties.put("hibernate.dialect", "org.hibernate.dialect.Oracle10gDialect");
		properties.put("hibernate.proc.param_null_passing", "true");
		properties.put("hibernate.show_sql", "true");
		return builder.dataSource(entidadDataSource).packages("pe.gob.servir.entidad.model")
				.persistenceUnit("entidad").properties(properties).build();
	}

	@Primary
	@Bean(name = "entidadTransactionManager")
	public PlatformTransactionManager transactionManager(
			@Qualifier("entidadEntityManagerFactory") EntityManagerFactory entidadEntityManagerFactory) {
		return new JpaTransactionManager(entidadEntityManagerFactory);
	}
}
