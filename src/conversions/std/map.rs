use std::{cmp, collections, hash};

#[cfg(feature = "experimental-inspect")]
use crate::inspect::types::TypeInfo;
use crate::{
    conversion::{FromPyObjectOwned, IntoPyObject},
    instance::Bound,
    types::{any::PyAnyMethods, dict::PyDictMethods, IntoPyDict, PyDict},
    Borrowed, BoundObject, FromPyObject, IntoPy, PyAny, PyErr, PyObject, Python, ToPyObject,
};

impl<K, V, H> ToPyObject for collections::HashMap<K, V, H>
where
    K: hash::Hash + cmp::Eq + ToPyObject,
    V: ToPyObject,
    H: hash::BuildHasher,
{
    fn to_object(&self, py: Python<'_>) -> PyObject {
        IntoPyDict::into_py_dict(self, py).into()
    }
}

impl<K, V> ToPyObject for collections::BTreeMap<K, V>
where
    K: cmp::Eq + ToPyObject,
    V: ToPyObject,
{
    fn to_object(&self, py: Python<'_>) -> PyObject {
        IntoPyDict::into_py_dict(self, py).into()
    }
}

impl<K, V, H> IntoPy<PyObject> for collections::HashMap<K, V, H>
where
    K: hash::Hash + cmp::Eq + IntoPy<PyObject>,
    V: IntoPy<PyObject>,
    H: hash::BuildHasher,
{
    fn into_py(self, py: Python<'_>) -> PyObject {
        let iter = self
            .into_iter()
            .map(|(k, v)| (k.into_py(py), v.into_py(py)));
        IntoPyDict::into_py_dict(iter, py).into()
    }

    #[cfg(feature = "experimental-inspect")]
    fn type_output() -> TypeInfo {
        TypeInfo::dict_of(K::type_output(), V::type_output())
    }
}

impl<'py, K, V, H> IntoPyObject<'py> for collections::HashMap<K, V, H>
where
    K: IntoPyObject<'py> + cmp::Eq + hash::Hash,
    V: IntoPyObject<'py>,
    H: hash::BuildHasher,
{
    type Target = PyDict;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let dict = PyDict::new(py);
        for (k, v) in self {
            dict.set_item(
                k.into_pyobject(py).map_err(Into::into)?.into_bound(),
                v.into_pyobject(py).map_err(Into::into)?.into_bound(),
            )?;
        }
        Ok(dict)
    }
}

impl<'a, 'py, K, V, H> IntoPyObject<'py> for &'a collections::HashMap<K, V, H>
where
    &'a K: IntoPyObject<'py> + cmp::Eq + hash::Hash,
    &'a V: IntoPyObject<'py>,
    H: hash::BuildHasher,
{
    type Target = PyDict;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let dict = PyDict::new(py);
        for (k, v) in self {
            dict.set_item(
                k.into_pyobject(py).map_err(Into::into)?.into_bound(),
                v.into_pyobject(py).map_err(Into::into)?.into_bound(),
            )?;
        }
        Ok(dict)
    }
}

impl<K, V> IntoPy<PyObject> for collections::BTreeMap<K, V>
where
    K: cmp::Eq + IntoPy<PyObject>,
    V: IntoPy<PyObject>,
{
    fn into_py(self, py: Python<'_>) -> PyObject {
        let iter = self
            .into_iter()
            .map(|(k, v)| (k.into_py(py), v.into_py(py)));
        IntoPyDict::into_py_dict(iter, py).into()
    }

    #[cfg(feature = "experimental-inspect")]
    fn type_output() -> TypeInfo {
        TypeInfo::dict_of(K::type_output(), V::type_output())
    }
}

impl<'py, K, V> IntoPyObject<'py> for collections::BTreeMap<K, V>
where
    K: IntoPyObject<'py> + cmp::Eq,
    V: IntoPyObject<'py>,
{
    type Target = PyDict;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let dict = PyDict::new(py);
        for (k, v) in self {
            dict.set_item(
                k.into_pyobject(py).map_err(Into::into)?.into_bound(),
                v.into_pyobject(py).map_err(Into::into)?.into_bound(),
            )?;
        }
        Ok(dict)
    }
}

impl<'a, 'py, K, V> IntoPyObject<'py> for &'a collections::BTreeMap<K, V>
where
    &'a K: IntoPyObject<'py> + cmp::Eq,
    &'a V: IntoPyObject<'py>,
{
    type Target = PyDict;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let dict = PyDict::new(py);
        for (k, v) in self {
            dict.set_item(
                k.into_pyobject(py).map_err(Into::into)?.into_bound(),
                v.into_pyobject(py).map_err(Into::into)?.into_bound(),
            )?;
        }
        Ok(dict)
    }
}

impl<'py, K, V, S> FromPyObject<'_, 'py> for collections::HashMap<K, V, S>
where
    K: FromPyObjectOwned<'py> + cmp::Eq + hash::Hash,
    V: FromPyObjectOwned<'py>,
    S: hash::BuildHasher + Default,
{
    fn extract(ob: Borrowed<'_, 'py, PyAny>) -> Result<Self, PyErr> {
        let dict = ob.downcast::<PyDict>()?;
        let mut ret = collections::HashMap::with_capacity_and_hasher(dict.len(), S::default());
        for (k, v) in dict.iter() {
            ret.insert(k.extract()?, v.extract()?);
        }
        Ok(ret)
    }

    #[cfg(feature = "experimental-inspect")]
    fn type_input() -> TypeInfo {
        TypeInfo::mapping_of(K::type_input(), V::type_input())
    }
}

impl<'py, K, V> FromPyObject<'_, 'py> for collections::BTreeMap<K, V>
where
    K: FromPyObjectOwned<'py> + cmp::Ord,
    V: FromPyObjectOwned<'py>,
{
    fn extract(ob: Borrowed<'_, 'py, PyAny>) -> Result<Self, PyErr> {
        let dict = ob.downcast::<PyDict>()?;
        let mut ret = collections::BTreeMap::new();
        for (k, v) in dict.iter() {
            ret.insert(k.extract()?, v.extract()?);
        }
        Ok(ret)
    }

    #[cfg(feature = "experimental-inspect")]
    fn type_input() -> TypeInfo {
        TypeInfo::mapping_of(K::type_input(), V::type_input())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, HashMap};

    #[test]
    fn test_hashmap_to_python() {
        Python::with_gil(|py| {
            let mut map = HashMap::<i32, i32>::new();
            map.insert(1, 1);

            let m = map.to_object(py);
            let py_map = m.downcast_bound::<PyDict>(py).unwrap();

            assert!(py_map.len() == 1);
            assert!(
                py_map
                    .get_item(1)
                    .unwrap()
                    .unwrap()
                    .extract::<i32>()
                    .unwrap()
                    == 1
            );
            assert_eq!(map, py_map.extract().unwrap());
        });
    }

    #[test]
    fn test_btreemap_to_python() {
        Python::with_gil(|py| {
            let mut map = BTreeMap::<i32, i32>::new();
            map.insert(1, 1);

            let m = map.to_object(py);
            let py_map = m.downcast_bound::<PyDict>(py).unwrap();

            assert!(py_map.len() == 1);
            assert!(
                py_map
                    .get_item(1)
                    .unwrap()
                    .unwrap()
                    .extract::<i32>()
                    .unwrap()
                    == 1
            );
            assert_eq!(map, py_map.extract().unwrap());
        });
    }

    #[test]
    fn test_hashmap_into_python() {
        Python::with_gil(|py| {
            let mut map = HashMap::<i32, i32>::new();
            map.insert(1, 1);

            let m: PyObject = map.into_py(py);
            let py_map = m.downcast_bound::<PyDict>(py).unwrap();

            assert!(py_map.len() == 1);
            assert!(
                py_map
                    .get_item(1)
                    .unwrap()
                    .unwrap()
                    .extract::<i32>()
                    .unwrap()
                    == 1
            );
        });
    }

    #[test]
    fn test_btreemap_into_py() {
        Python::with_gil(|py| {
            let mut map = BTreeMap::<i32, i32>::new();
            map.insert(1, 1);

            let m: PyObject = map.into_py(py);
            let py_map = m.downcast_bound::<PyDict>(py).unwrap();

            assert!(py_map.len() == 1);
            assert!(
                py_map
                    .get_item(1)
                    .unwrap()
                    .unwrap()
                    .extract::<i32>()
                    .unwrap()
                    == 1
            );
        });
    }
}
